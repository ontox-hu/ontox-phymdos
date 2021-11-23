##################################################
## MINERVA API version: 14.0
## Script purpose: An example of MINERVA API calls adding new project
## Date: 23/03/2020
## Author: Piort Gawron (piotr.gawron@uni.lu)
## MANUAL: https://minerva.pages.uni.lu/doc/api/14.0/projects/
##################################################
import webbrowser
import requests
import os
import json
import time
# import pandas
# from io import StringIO

session = requests.Session()

### INPUT YOUR CREDENTIALS TO LOGIN to PD map instance
login = "admin"
password = "admin"
api_url = "http://localhost:8080/minerva/api"
map_file = "physmap.xml"

# LOGIN to PD map instance with your credentials
login_request = session.post(api_url+"/doLogin", data = {'login': login, 'password': password})
print(login_request.text, "\n")
print(session.cookies.get_dict(), "\n")

# REMOVE previous map opened in same session (if it exists )
if 'project_id' in locals():
  delete_project = session.delete(api_url+"/projects/"+project_id)

# # GET logs of given project
# get_project_logs = session.get(api_url+"/projects/"+"neural"+"/logs/")
# json = get_project_logs.json
# data = StringIO(get_project_logs.text)
# df = pandas.(data)

# UPLOAD FILE TO THE SYSTEM
stat_info = os.stat(map_file)
with open(map_file) as f: file_content = f.read()

# Before creating a new project in MINERVA, source file 'map_file' must be uploaded to the instance (MANUAL: https://minerva.pages.uni.lu/doc/api/14.0/files/)
# Allocate memory in the system for 'map_file', which length is 'stat_info.st_size' bytes
create_file_request = session.post(api_url+"/files/", data = {'filename': map_file, 'length': stat_info.st_size})

# Get the information about the uploaded file: the 'id' is necessary to upload the file's content.
content = json.loads(create_file_request.text)

file_id = content["id"]
# Upload file's content to the instance
upload_content_request = session.post(api_url+"/files/"+str(file_id)+":uploadContent", data = file_content)


# CREATE THE PROJECT
project_id = "physmap_"+str(file_id)
r.project_id = "physmap_"+str(file_id)
project_name = "Physmap"
create_map_request = session.post(api_url+"/projects/"+project_id, data = {"file-id":file_id, "name": project_name , "version": "example", "mapCanvasType":"OPEN_LAYERS", "parser":"lcsb.mapviewer.converter.model.sbml.SbmlParser"})

time.sleep(1)

# GET LIST of all (sub)maps in the project
get_submaps_request = session.get(api_url+"/projects/"+project_id+"/models/")
content  = json.loads(get_submaps_request.text)
# In this example there is only one map uploaded
submap_id = content[0]['idObject']

# PUT A COMMENT to the map pinned to the coordinates (100.00,100.00)
create_comment_request = session.post(api_url+"/projects/"+project_id+"/comments/models/"+str(submap_id)+"/points/100.00,100.00", data = {"name":"Your Name", "email":"your_email@dot.com", "content":"automatically generated content","pinned":"true"})

print("Comment details:", create_comment_request.text)

# GIVE PERMISSION to users to open the map
grant_user_permission = session.patch(api_url+"/projects/"+project_id+":grantPrivileges", data = '[{"login":"anonymous", "privilegeType":"READ_PROJECT"}]')

# OPEN the map in the webbrowser
time.sleep(2)
webbrowser.open("http://localhost:8080/minerva/index.xhtml?id="+project_id)
