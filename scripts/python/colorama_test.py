#!/usr/bin/env python3
##
from colorama import init, Fore, Back, Style

def get_color_code(color_value: str) -> str:
    return color_value.replace('\x1b[', '').replace('m', '')

def print_colors(colors: dict, label: str) -> None:
    print(f"\n{label}:")
    for color_name, color_value in colors.items():
        if isinstance(color_value, str) and color_value.startswith('\x1b['):
            color_code = get_color_code(color_value)
            print(f"{color_value}{color_name} ({color_code})")

# Initialize colorama
init()

# Print foreground colors
print_colors(Fore.__dict__, "Foreground colors")
print(Fore.RESET)

# Print background colors
print_colors(Back.__dict__, "Background colors")
print(Back.RESET)

# Print text styles
print_colors(Style.__dict__, "Text styles")
print(Style.RESET_ALL)
