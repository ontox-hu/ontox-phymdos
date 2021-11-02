def sbtab_to_sbml(file):
  import sbtab
  import libsbml
  from sbtab import SBtab
  from sbtab import validatorSBtab
  from sbtab import sbtab2sbml
  from pathlib import Path
  
  # Read in TSV file containing SBtab info and definitions file
  sbtab_file   = open(file,'r')
  file_content = sbtab_file.read()
  sbtab_file.close()
  
  # Make SBtab object
  Sd = SBtab.SBtabDocument()
  Sd.set_filename('physmap.tsv')
  Sd.set_name('physmap')
  Sd.add_sbtab_string(file_content, filename='physmap.tsv')
  
  # validate all SBtabs from the document
  ValidateDocument = validatorSBtab.ValidateDocument(Sd) #, def_tabl
  warnings = ValidateDocument.validate_document()
  print(warnings)
  
  # Convert SBtab file to SBML
  Cd = sbtab2sbml.SBtabDocument(Sd)
  (sbml, warnings) = Cd.convert_to_sbml('24')
  
  #r.sbml_string = sbml
  
  # # Save SBML xml file (overwrites existing file with the same name)
  # myfile = open("physmap.xml", "w")
  # myfile.write(sbml)
  # myfile.close()

def sbml_to_sbtab(file):
  import sbtab
  import libsbml
  from sbtab import sbml2sbtab
  from pathlib import Path
  
  # read SBML model
  f = open(file, 'r').read()
  reader = libsbml.SBMLReader()
  doc = reader.readSBMLFromString(f)
  model = doc.getModel()
      
  # convert model to SBtab Document Sd
  # (Od is an ObjTable Document in SBtab format)
  Cd = sbml2sbtab.SBMLDocument(model, file)
  (Sd, Od, warnings) = Cd.convert_to_sbtab()
  
  r.sbtab_string = Sd.to_str()
