import sbtab
import libsbml
from sbtab import SBtab
from sbtab import validatorSBtab
from sbtab import sbtab2sbml
from pathlib import Path

from SBtab import SBtabTable
from validatorSBtab import ValidateTable
from sbtab2sbml import SBtabDocument

# Read in TSV file containing SBtab info and definitions file
sbtab_file   = open("physmap.tsv")
file_content = sbtab_file.read()
sbtab_file.close()

def_file = open("definitions.tsv", "r")
def_file_content = def_file.read()
def_file.close()

# Make SBtab object
Sd = SBtab.SBtabDocument()
Sd.set_filename('physmap.tsv')
Sd.set_name('physmap')
Sd.add_sbtab_string(file_content,filename='physmap.tsv')

def_tabl = SBtab.SBtabDocument()
def_tabl.set_filename('definitions.tsv')
def_tabl.set_filename('definitions')
Sd.add_sbtab_string(def_file_content,filename='definitions.tsv')

# Validate the SBtab file and save warnings
V_obj = validatorSBtab.ValidateDocument(Sd) #, def_tabl
warnings = V_obj.validate_document()
print(warnings)

# Convert SBtab file to SBML
Cd = sbtab2sbml.SBtabDocument(Sd)
(sbml, warnings) = Cd.convert_to_sbml('24')

# Save SBML xml file (overwrites existing file with the same name)
myfile = open("physmap.xml", "w")
myfile.write(sbml)
myfile.close()
