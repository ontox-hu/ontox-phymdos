ONTOX-PMDEP - the ONTOX Physiological Map Data Entry Portal
============================================================

This application was created to serve as a bridge between the SBTAB and SBML formats. Its core functionality is to allow the user to easily create SBML style physiological maps from litarature by filling out easy to use datatables and converting them to the desired format by the click of a button. <br>
The webversion of the application can be found [here](https://datascience.hu.nl/rsconnect/pmdep/).

---
<ol>
  <li>
  Upon opening the app users will first be guided to the homescreen where they are presented with 3 choices. 
  </li>
  <br>
  
  ![home](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/home.png/)
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
    Upon clicking one of the uploading options users will be presented an uploading screen in which they can upload eiter a .tsv file for SBtab or a .xml file for SBML. 
  </li>
  <br>
  
  ![upload](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/upload.png/)
  
  <li>
  Once the upload is complete and the continue button is clicked the app will open the tables corresponding to the upload file, and users will be redirected to the first setup where they can enter a name for the document and choose a SBtab version (change only if needed, the newest version is standard).
  </li>
  <br>
  
  ![setup](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/first_setup_upload.png/)
  
  <li>
    Clicking 'save input' will redirect users to the 'select tables' tab which can also be accessed from the menu on the left side of the screen. Here users can add or remove tables from the menu. Please note that removing a table from the menu deletes any data that is inside the table.
  </li>
  <br>
  
  ![add](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/add_upload.png)
  
  <li>
    Once all the correct tables are sellected users can start filling the new tables or adding to the uploaded tables however they see fit. 
  </li>
  <br> 
  
  ![table](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/table_upload.png)
  
  For empty tables the first step is to start adding colums to the table. This can also be done on existing tables to add information.
  <br>
  
  ![choose](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/column_choose.png)
    
  To find out what columns users can view the description of table elements found at the bottom of the page.
  <br>
  
  ![description](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/table_description.png)
  
  <li> 
    When users are done filling out the tables they can click the button to go to the download screen or they can click back to Setup on the menu on the left. Here they can export the file to SBtab format as well as SBML. 
  </li>
  <br>
  
  ![download](https://github.com/ontox-hu/ontox-pmdep/blob/dev_daniel/documentation/download.png)
  
</ol>
