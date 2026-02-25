from modeller import *
from modeller.automodel import *


class MyModel(automodel):
    def select_atoms(self):
	return selection(self) - selection(self).only_atom_types('CA')
