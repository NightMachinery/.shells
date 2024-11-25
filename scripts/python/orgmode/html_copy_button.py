#!/usr/bin/env python3
from bs4 import BeautifulSoup
import sys

# Constants for UI elements and styling
UI_CONFIG = {
    'icons': {
        'copy': '''<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 256 256">
            <path d="M200,32H163.74a47.92,47.92,0,0,0-71.48,0H56A16,16,0,0,0,40,48V216a16,16,0,0,0,16,16H200a16,16,0,0,0,16-16V48A16,16,0,0,0,200,32Zm-72,0a32,32,0,0,1,32,32H96A32,32,0,0,1,128,32Zm72,184H56V48H82.75A47.93,47.93,0,0,0,80,64v8a8,8,0,0,0,8,8h80a8,8,0,0,0,8-8V64a47.93,47.93,0,0,0-2.75-16H200Z"></path>
        </svg>''',
        'success': '''<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 256 256">
            <path d="M104,192a8.5,8.5,0,0,1-5.7-2.3l-56-56a8.1,8.1,0,0,1,11.4-11.4L104,172.7,202.3,74.3a8.1,8.1,0,0,1,11.4,11.4l-104,104A8.5,8.5,0,0,1,104,192Z"></path>
        </svg>''',
        'error': '''<svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" fill="currentColor" viewBox="0 0 256 256">
            <path d="M205.7,197.7,133.7,125.7a8,8,0,0,1,0-11.4L205.7,42.3a8.1,8.1,0,0,0-11.4-11.4L122.3,102.3a8,8,0,0,1-11.4,0L38.9,30.9A8.1,8.1,0,0,0,27.5,42.3L99.5,114.3a8,8,0,0,1,0,11.4L27.5,197.7a8.1,8.1,0,0,0,11.4,11.4l72-72a8,8,0,0,1,11.4,0l72,72a8.1,8.1,0,0,0,11.4-11.4Z"></path>
        </svg>'''
    },
    'classes': {
        'prefix': 'code',  # DRY: Common prefix for all classes
        'tooltip': 'tooltip'
    },
    'timing': {
        'transition': '0.2s',
        'feedback': 2000
    },
    'theme': {
        'button': {
            'size': '32px',
            'radius': '6px'
        }
    },
    'text': {
        'tooltip': 'Copy contents',
        'copied': 'Copied!',
        'error': 'Failed to copy'
    }
}

# DRY: Generate consistent class names
CLASS_NAMES = {
    'button': f"{UI_CONFIG['classes']['prefix']}-copy-button",
    'success': f"{UI_CONFIG['classes']['prefix']}-copy-success",
    'error': f"{UI_CONFIG['classes']['prefix']}-copy-error",
    'tooltip': UI_CONFIG['classes']['tooltip']
}

def main():
    import argparse
    parser = argparse.ArgumentParser(description='Add a stylish "Copy to clipboard" button to code blocks in HTML.')
    parser.add_argument('--input', default='-', help='Input HTML file (default: stdin)')
    parser.add_argument('--output', default='-', help='Output HTML file (default: stdout)')
    args = parser.parse_args()

    # Read input HTML
    if args.input == '-':
        input_html = sys.stdin.read()
    else:
        with open(args.input, 'r', encoding='utf-8') as f:
            input_html = f.read()

    # Parse HTML
    soup = BeautifulSoup(input_html, 'html.parser')

    # Find all pre elements with class 'src'
    pre_elements = soup.find_all('pre', class_='src')

    # For each pre element, add a copy button
    for pre in pre_elements:
        # Ensure pre has position relative for button positioning
        pre_style = 'position: relative;'
        if pre.get('style'):
            pre_style += ';' + pre['style']
        pre['style'] = pre_style

        # Create the copy button with tooltip attribute
        button = soup.new_tag('button', **{
            'class': CLASS_NAMES['button'],
            'type': 'button',
            'aria-label': UI_CONFIG['text']['tooltip'],
            'data-tooltip': UI_CONFIG['text']['tooltip']
        })

        # Create the SVG element using BeautifulSoup
        svg_soup = BeautifulSoup(UI_CONFIG['icons']['copy'], 'html.parser')
        button.append(svg_soup.svg)

        # Insert the button as the first child of pre
        pre.insert(0, button)

    # Generate CSS with better organization and DRY principles
    css_code = f'''
    /* CSS Variable definitions for theming and reuse */
    :root {{
        /* Colors */
        --color-bg-base: rgb(243 244 246);
        --color-bg-hover: rgb(229 231 235 / 0.4);
        --color-bg-active: rgb(229 231 235);
        --color-text-base: rgb(107 114 128);
        --color-text-hover: rgb(17 24 39);
        --color-success: rgb(34 197 94);
        --color-error: rgb(239 68 68);

        /* Ring */
        --ring-color: rgb(99 102 241);
        --ring-offset: 2px;

        /* Animation */
        --transition-timing: {UI_CONFIG['timing']['transition']};

        /* Tooltip */
        --tooltip-bg: rgb(55 65 81);
        --tooltip-text: rgb(255 255 255);
    }}

    /* Base button styles */
    .{CLASS_NAMES['button']} {{
        position: absolute;
        top: 0;
        right: 0;
        height: {UI_CONFIG['theme']['button']['size']};
        width: {UI_CONFIG['theme']['button']['size']};
        display: inline-flex;
        align-items: center;
        justify-content: center;
        border-radius: {UI_CONFIG['theme']['button']['radius']};
        border: none;
        background-color: var(--color-bg-base);
        color: var(--color-text-base);
        cursor: pointer;
        transition: all var(--transition-timing) ease;
        z-index: 1; /* Raise button above other elements */
    }}

    /* Button states */
    .{CLASS_NAMES['button']}:hover {{
        background-color: var(--color-bg-hover);
        color: var(--color-text-hover);
    }}

    .{CLASS_NAMES['button']}:active {{
        transform: scale(0.95);
        background-color: var(--color-bg-active);
    }}

    .{CLASS_NAMES['button']}:focus-visible {{
        outline: none;
        box-shadow: 0 0 0 var(--ring-offset) var(--color-bg-base),
                   0 0 0 calc(var(--ring-offset) + 1px) var(--ring-color);
    }}

    .{CLASS_NAMES['button']}:disabled {{
        opacity: 0.5;
        pointer-events: none;
        box-shadow: none;
    }}

    /* Status states */
    .{CLASS_NAMES['button']}.{CLASS_NAMES['success']} {{
        color: var(--color-success);
    }}

    .{CLASS_NAMES['button']}.{CLASS_NAMES['error']} {{
        color: var(--color-error);
    }}

    /* SVG styling */
    .{CLASS_NAMES['button']} svg {{
        fill: currentColor;
    }}

    /* Tooltip styling */
    .{CLASS_NAMES['button']}::before {{
        content: attr(data-tooltip);
        position: absolute;
        bottom: calc(100% + 5px);
        right: 50%;
        transform: translateX(50%) scale(0.8);
        padding: 5px 10px;
        border-radius: 4px;
        background-color: var(--tooltip-bg);
        color: var(--tooltip-text);
        font-size: 12px;
        white-space: nowrap;
        opacity: 0;
        pointer-events: none;
        transition: all var(--transition-timing) ease;
        z-index: 2; /* Ensure tooltip is on top */
    }}

    .{CLASS_NAMES['button']}:hover::before {{
        opacity: 1;
        transform: translateX(50%) scale(1);
    }}
    '''

    # Generate JavaScript with status message handling
    script_code = f'''
    function copyCodeToClipboard(button) {{
        const codeBlock = button.closest('pre');
        const code = Array.from(codeBlock.childNodes)
            .filter(node => node !== button)
            .map(node => node.textContent)
            .join('');

        navigator.clipboard.writeText(code).then(
            function() {{
                button.classList.add('{CLASS_NAMES["success"]}');
                button.innerHTML = `{UI_CONFIG['icons']['success']}`;
                button.setAttribute('data-tooltip', '{UI_CONFIG['text']['copied']}');

                setTimeout(function() {{
                    button.classList.remove('{CLASS_NAMES["success"]}');
                    button.innerHTML = `{UI_CONFIG['icons']['copy']}`;
                    button.setAttribute('data-tooltip', '{UI_CONFIG['text']['tooltip']}');
                }}, {UI_CONFIG['timing']['feedback']});
            }},
            function() {{
                button.classList.add('{CLASS_NAMES["error"]}');
                button.innerHTML = `{UI_CONFIG['icons']['error']}`;
                button.setAttribute('data-tooltip', '{UI_CONFIG['text']['error']}');

                setTimeout(function() {{
                    button.classList.remove('{CLASS_NAMES["error"]}');
                    button.innerHTML = `{UI_CONFIG['icons']['copy']}`;
                    button.setAttribute('data-tooltip', '{UI_CONFIG['text']['tooltip']}');
                }}, {UI_CONFIG['timing']['feedback']});
            }}
        );
    }}

    document.addEventListener('DOMContentLoaded', function() {{
        const copyButtons = document.querySelectorAll('.{CLASS_NAMES["button"]}');
        copyButtons.forEach(button => {{
            button.addEventListener('click', () => copyCodeToClipboard(button));
        }});
    }});
    '''

    # Insert CSS into <head>
    style_tag = soup.new_tag('style')
    style_tag.string = css_code
    if soup.head:
        soup.head.append(style_tag)
    else:
        head_tag = soup.new_tag('head')
        head_tag.append(style_tag)
        soup.html.insert(0, head_tag)

    # Check if script is already present to avoid duplication
    existing_scripts = soup.find_all('script')
    script_already_present = any('copyCodeToClipboard' in (script.string or '') for script in existing_scripts)

    if not script_already_present:
        script_tag = soup.new_tag('script')
        script_tag.string = script_code
        if soup.body:
            soup.body.append(script_tag)
        else:
            soup.append(script_tag)

    # Output the modified HTML
    output_html = soup.prettify()
    if args.output == '-':
        print(output_html)
    else:
        with open(args.output, 'w', encoding='utf-8') as f:
            f.write(output_html)

if __name__ == '__main__':
    main()
