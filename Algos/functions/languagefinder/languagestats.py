import json
import requests

url = "https://api.github.com/users/Vulpe-Fox/repos"

def get_repos():
    response = requests.get(url)
    repos = json.loads(response.text)
    return repos

def get_lang_urls(repos):
    lang_urls = []
    for repo in repos:
        lang_urls.append(repo["languages_url"])
    return lang_urls

def get_langs(lang_urls):
    langdict = {}
    for lang_url in lang_urls:
        response = requests.get(lang_url)
        langs = json.loads(response.text)
        for lang in langs:
            if langdict.get(lang):
                curr_val = langdict.get(lang)
                langdict[lang] = curr_val + langs[lang]
            else:
                langdict[lang] = langs[lang]
    return langdict

def find_dist(langs):
    total = 0
    for lang in langs:
        total += int(langs[lang])
    for lang in langs:
        langs[lang] = "{percent}%".format(percent = langs[lang]/total*100)
    return langs

def main():
    repos_data = get_repos()
    lang_urls = get_lang_urls(repos_data)
    langdict = get_langs(lang_urls)
    dist = find_dist(langdict)
    
    with open('langs.json', 'w') as fp:
        json.dump(dist, fp)
    
    print(dist)


if __name__ == "__main__":
    main() 