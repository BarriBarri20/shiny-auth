"""project URL Configuration

The `urlpatterns` list routes URLs to views. For more information please see:
    https://docs.djangoproject.com/en/4.1/topics/http/urls/
Examples:
Function views
    1. Add an import:  from my_app import views
    2. Add a URL to urlpatterns:  path('', views.home, name='home')
Class-based views
    1. Add an import:  from other_app.views import Home
    2. Add a URL to urlpatterns:  path('', Home.as_view(), name='home')
Including another URLconf
    1. Import the include() function: from django.urls import include, path
    2. Add a URL to urlpatterns:  path('blog/', include('blog.urls'))
"""
from django.contrib import admin
from django.urls import path
from . import views
from django.views.decorators.csrf import csrf_exempt

urlpatterns = [
    path("admin/", admin.site.urls,),
    path("shiny/", views.shiny, name="shiny"),
    path("shiny_contents/", views.shiny_contents, name="shiny_contents"),
    path("shiny_auth/", views.shiny_auth),
]
# Here I identify all the urls that will appear to the user and I assign them to the corresponding view
# So you can modify the urls to your liking
# admin is for controlling the website where you can add users, etc.
