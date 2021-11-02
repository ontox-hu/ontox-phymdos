def open_sbml(file):
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
  
  r.sbml_file = Sd.to_str()
