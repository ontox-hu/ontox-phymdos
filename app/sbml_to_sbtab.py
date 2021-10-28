import sbtab
import libsbml
from sbtab import SBtab
from sbtab import validatorSBtab
from sbtab import sbml2sbtab
from pathlib import Path

# Read in TSV file containing SBtab info and definitions file
sbtab_file   = open(r.file_test,'r')
file_content = sbtab_file.read()
sbtab_file.close()

# Make SBtab object
Sd = SBtab.SBtabDocument()
Sd.set_filename('physmap.tsv')
Sd.set_name('physmap')
#Sd.add_sbtab_string(file_content, filename='physmap.tsv')

# validate all SBtabs from the document
#ValidateDocument = validatorSBtab.ValidateDocument(Sd) #, def_tabl
#warnings = ValidateDocument.validate_document()
#print(warnings)

# read SBML model
f = open('app/liver_bile_template.xml', 'r').read()
reader = libsbml.SBMLReader()
doc = reader.readSBMLFromString(f)
model = doc.getModel()
    
# convert model to SBtab Document Sd
# (Od is an ObjTable Document in SBtab format)
Cd = sbml2sbtab.SBMLDocument(model, 'app/liver_bile_template.xml')
(Sd, Od, warnings) = Cd.convert_to_sbtab()

# save SBTab TSV file (overwrites existing file with the same name)
myfile = open("physmap.tsv", "w")
myfile.write(Sd)
myfile.close()
