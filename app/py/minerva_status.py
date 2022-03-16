# Get MINERVA session status while uploading
def get_status():
  api_url = "http://145.38.204.52:8080/minerva/api"
  project_id = r.project_id
  session_info = session.get(api_url+"/projects/"+project_id)
  text = json.loads(session_info.text)
  return "Status: "+ text["status"], "Progress: "+ str(round(text["progress"], 2))+ "%", text["progress"]
