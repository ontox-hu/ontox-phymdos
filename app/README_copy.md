ONTOX-PMDEP - the ONTOX Physiological Map Data Entry Portal
============================================================

<font size="+1">
This application was created to serve as a bridge between [sysrev.com](https://sysrev.com/) and [Celldesigner](http://www.celldesigner.org/). Its core functionality is to allow the user to easily create SBML style physiological maps from litarature by filling out easy to use datatables and converting them to the desired format by the click of a button. 
The format used to create these datatables is SBtab, which is essentially a .tsv file containing several tables that together define a system. These tables can be filled out at sysrev.com and in this app and can then be converted to SBML files that are used by Celldesigner to build physiological maps. This workflow has been illustrated below. <br>
<img src="app_workflow.png" width="80%" height="100%" />
<hr style="height:2px;border-width:0;color:gray;background-color:gray">
The MINERVA functionality is not yet operable on the web-version of the app.
<hr style="height:2px;border-width:0;color:gray;background-color:gray">
<ol>
  <li>
  Upon opening the app, users will first be guided to the homescreen where they are presented with 3 choices. 
  </li>
  <br>
  
  <img src="home.png" width="100%" height="100%" />
  
  <br> <br>
  <ol type=a>
    <li> 
      Create new SBtab: <br>
      Creating a new SBtab will allow users to create their own SBtab from scratch. (For creating new maps, skip point 2)
      </li>
    <li> 
      Upload an SBtab object: <br>
      Uploading an SBtab object will allow users to upload a .tsv containing an SBtab document. The tabs within the .tsv will be opened in the app.
      </li>
    <li>
      Upload an SBML object: <br>
      Uploading an SBML object will allow users to upload a .xml containing an SBML document. The SBML will be converted to SBtab and the tabs within the SBtab will be opened in the app.
      </li>
   </ol>
  <br>
  <li>
    Upon clicking one of the uploading options, users will be presented an uploading screen in which they can upload either a .tsv file for SBtab or a .xml file for SBML. 
  </li>
  <br>
  
  <img src="upload.png" width="100%" height="100%" />

  <br> <br>
  <li>
  Once the upload is complete and the continue button is clicked the app will open the tables corresponding to the upload file, and users will be redirected to the first setup where they can enter a name for the document and choose a SBtab version (change only if needed, the newest version is standard).
  </li>
  <br>
  
  <img src="first_setup_upload.png" width="100%" height="100%" />

  <br> <br>
  <li>
    Clicking 'save input' will redirect users to the 'select tables' tab which can also be accessed from the menu on the left side of the screen. Here users can add or remove tables from the menu. Please note that removing a table from the menu deletes any data that is inside the table.
  </li>
  <br>
  
  <img src="add_upload.png" width="100%" height="100%" />

  <br> <br>
  <li>
    Once all the correct tables are selected users can start filling the new tables or adding to the uploaded tables however they see fit. 
  </li>
  <br> 
  
  <img src="table_upload.png" width="100%" height="100%" />

  <br> <br>
  For empty tables the first step is to start adding colums to the table. This can also be done on existing tables to add information.
  <br> <br>
  
  <img src="column_choose.png" width="100%" height="100%" />

  <br> <br>
  To find out what columns, users can view the description of table elements found at the bottom of the page.
  <br> <br>
  
  <img src="table_description.png" width="100%" height="100%" />

  <br> <br>
  <li> 
    When users are done filling out the tables they can click the button to go to the download screen or they can click back to Setup on the menu on the left. Here they can export the file to SBtab format as well as SBML. 
  </li>
  <br>
  
  <img src="download.png" width="100%" height="100%" />

</ol>
</font>