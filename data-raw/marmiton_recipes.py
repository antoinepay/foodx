
# coding: utf-8

# In[12]:


import requests
from bs4 import BeautifulSoup
from tqdm import tqdm
import pandas as pd
from urllib.request import urlopen
import re


# In[2]:


def convert_to_url(str_input):
    return "https://www.marmiton.org" + str(str_input).split("<meta content=\"")[1].split("\" property=")[0]


# In[3]:


def get_random_urls(n):
    s="<meta content"
    urls_formatted_list=[]


    for i in tqdm(range(n)):
        response = requests.get("https://www.marmiton.org/recettes/recette-hasard.aspx")
        soup = BeautifulSoup(response.text,"lxml")
        soup=soup.find(property="og:url")
        if(s in str(soup)):
            url=convert_to_url(soup)
            if url not in urls_formatted_list:
                urls_formatted_list.append(url)
    
    return urls_formatted_list


# In[15]:


def get_recipe_data (URL_list):
    
    recipe_data = pd.DataFrame(columns=['URL','recipeTitle','recipeType',
                                    'recipeDifficulty','recipeCost','recipeBrand','recipeSelection',
                                   'recipePreparationTime','recipeCookingTime','recipeServings','recipeIngredients'])
    
    for url in tqdm(URL_list):
        soup = BeautifulSoup(urlopen(url).read(),"html.parser")

        targetScript = str(soup.find(type="text/javascript", text = re.compile("m_dataLayer")))

        recipeIngredients=targetScript.partition("recipeIngredients': ")[2].partition(",\r\n 'recipeType'")[0].strip("'")
        recipeType=targetScript.partition("recipeType': ")[2].partition(",\r\n 'recipeDifficulty'")[0].strip("'")
        recipeDifficulty = targetScript.partition("recipeDifficulty': ")[2].partition(",\r\n 'recipeCost'")[0].strip("'")
        recipeCost= targetScript.partition("recipeCost': ")[2].partition(",\r\n 'recipeBrand'")[0].strip("'")
        recipeBrand= targetScript.partition("recipeBrand': ")[2].partition(",\r\n 'recipeSelection'")[0].strip("'")
        recipeSelection= targetScript.partition("recipeSelection': ")[2].partition(",\r\n 'recipePreparationTime'")[0].strip("'")
        recipePreparationTime = targetScript.partition("recipePreparationTime': ")[2].partition(",\r\n 'recipeCookingTime'")[0].strip("'")
        recipeCookingTime = targetScript.partition("recipeCookingTime': ")[2].partition(",\r\n 'recipeServings'")[0].strip("'")
        recipeServings= targetScript.partition("recipeServings': ")[2].partition(",\r\n 'recipeTitle'")[0].strip("'")
        recipeTitle = targetScript.partition("recipeTitle': ")[2].partition("\r\n },\r\n")[0].strip("'")

        recipe_data=recipe_data.append({'URL':url,
                            'recipeTitle':recipeTitle,
                            'recipeType':recipeType,
                            'recipeDifficulty':recipeDifficulty,
                            'recipeCost':recipeCost,
                            'recipeBrand':recipeBrand,
                            'recipeSelection':recipeSelection,
                            'recipePreparationTime':recipePreparationTime,
                            'recipeCookingTime':recipeCookingTime,
                            'recipeServings':recipeServings,
                            'recipeIngredients':recipeIngredients},ignore_index=True)
    
    recipe_data.to_csv("new_recipe_data.csv",index=False)

