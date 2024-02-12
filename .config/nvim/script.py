#!/usr/bin/env python

import os
import argparse

EXIT_SUCCESS_CODE = 0
EXIT_FAILURE_CODE = 1


def new_modules(args: argparse.ArgumentParser):
    """
    Create a new module for neovim configuration
    """
    directory = args.directory or os.path.basename(directory)
    module_name = args.module_name
    module_path = os.path.join(directory, f"{module_name}.lua")

    print(f"Creating module '{module_path}'")
    if not os.path.exists(directory):
        os.makedirs(directory)

    if os.path.exists(module_path):
        print(f"The module '{module_path}' already exists")
        return EXIT_FAILURE_CODE
    else:
        template = f"""-- {module_name}.lua --------------------------------


-- {module_name}.lua ends here
"""
        with open(module_path, "w", encoding="utf-8") as f_out:
            f_out.write(template)

        return EXIT_SUCCESS_CODE


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(help="sub-command help")

    # create new module
    new_modules_parser = subparsers.add_parser("new", help="new help")
    new_modules_parser.add_argument("-d", dest="directory", type=str, default=None)
    new_modules_parser.add_argument("-t", dest="module_name", type=str)
    new_modules_parser.set_defaults(handler=new_modules)

    try:
        args = parser.parse_args()
        status_code = args.handler(args)
        exit(status_code)
    except Exception as e:
        print(e)
        exit(EXIT_FAILURE_CODE)
