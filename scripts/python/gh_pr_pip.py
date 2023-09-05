#!/usr/bin/env python3
##
import requests


def get_pr_details(user, repo, pr_number):
    url = f"https://api.github.com/repos/{user}/{repo}/pulls/{pr_number}"
    response = requests.get(url)
    data = response.json()

    if response.status_code == 200:
        return data["head"]["user"]["login"], data["head"]["ref"]
    else:
        print(f"Error: {data['message']}")
        return None, None


user, repo, pr_number = "gruns", "icecream", 152  # replace with your values
pr_user, pr_branch = get_pr_details(user, repo, pr_number)

if pr_user and pr_branch:
    print(f"pip install git+https://github.com/{pr_user}/{repo}.git@{pr_branch}")
