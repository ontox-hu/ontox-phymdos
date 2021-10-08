import sbtab
import libsbml
from sbtab import SBtab
from sbtab import validatorSBtab
from sbtab import sbtab2sbml
from pathlib import Path

# Read in TSV file containing SBtab info and definitions file
sbtab_file   = open(r.file_test,'r')
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

# Save SBML xml file (overwrites existing file with the same name)
myfile = open("physmap.xml", "w")
myfile.write(sbml)
myfile.close()
