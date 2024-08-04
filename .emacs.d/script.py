#!/usr/bin/env python3

import os
import argparse


EXIT_SUCCESS_CODE = 0
EXIT_FAILURE_CODE = 1



def create_modules_info(module_names: list[str], directory: str) -> list[str]:
    """
    Create a list of module paths
    """
    module_paths = []
    for module_name in module_names:
        module_path = os.path.join(os.getcwd(), directory, f"lolo-{module_name}.el")
        module_paths.append((module_name, module_path))
    return module_paths


def new_modules(args: argparse.ArgumentParser):
    """
    Create a new module for emacs configuration
    """
    directory = args.directory
    module_names = args.module_name

    if not os.path.exists(directory):
        os.makedirs(directory)
    modules_info = create_modules_info(module_names, directory)

    print(f"Creating modules: '{modules_info}'")

    for module_name, module_path in modules_info:
        if os.path.exists(module_path):
            print(f"The module '{module_path}' already exists")
        else:
            template = f""";; -*- coding: utf-8; lexical-binding: t -*-
;;
;;; Commentary:
;;
;;
;;; Code:


(provide 'lolo-{module_name})
;;; lolo-{module_name}.el ends here
"""
            with open(module_path, "w", encoding="utf-8") as f_out:
                f_out.write(template)

    return EXIT_SUCCESS_CODE


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    subparsers = parser.add_subparsers(help="sub-command help")

    # create new module
    parser.add_argument("-d", dest="directory", type=str, default="")
    parser.add_argument("-t", nargs="+", dest="module_name", type=str, required=True)
    parser.set_defaults(handler=new_modules)

    try:
        args = parser.parse_args()
        status_code = args.handler(args)
        exit(status_code)
    except Exception as e:
        print(e)
        exit(EXIT_FAILURE_CODE)
