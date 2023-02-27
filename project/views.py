from django.http import JsonResponse
from django.shortcuts import render
from django.http import HttpResponse

import requests

from bs4 import BeautifulSoup  # you need to install this library


# this is here to make sure that the web page is loaded I mean where the
# main page will be loaded and the shiny app will be deployed as they can seeit


def shiny(request):
    return render(request, "index.html")


# here we access the shiny app and we get the html code
# make sure to run the shiny app in the background
# you can do this by running the following command in the terminal
# R -e "shiny::runApp('path/to/shiny/app')" and you can specify the port
# in which the shiny app will be deployed
def shiny_contents(request):
    response = requests.get("http://localhost:8100")
    soup = BeautifulSoup(response.content, "html.parser")

    return JsonResponse({"html_contents": str(soup)})


# this is for the authentication of the user; you can modify this to your liking
# I mean making the logic and needed priveleges for the user
def shiny_auth(request):
    if request.user.is_authenticated:
        if request.user.is_superuser:
            return HttpResponse(status=200)
    return HttpResponse(status=403)


# now will go to visit the template page that will be rendered in the browser : index.html
# now we are going to modify/create the necessary files for the nginx server
# we will create a file inside the server root for now you can modify the necessary commends as mentioned in the tutorial
# https://pawamoy.github.io/posts/django-auth-server-for-shiny/
# also you need to check that the nginx server is running correctly by typping the following command in the terminal
# you need to restart it if it is not running
# sudo systemctl restart nginx
# sudo nginx -t
# if you get the following message then you are good to go
# nginx: the configuration file /etc/nginx/nginx.conf syntax is ok
# also check the status of the nginx server by typing the following command in the terminal
# sudo systemctl status nginx
