#!/usr/bin/env python3

import panflute as pf


def action(elem, doc):
    if isinstance(elem, pf.Link):
        return list(elem.content)

def main(doc=None):
	return pf.run_filter(action, doc=doc)

if __name__ == '__main__':
    main()
