#include <iostream>

// Include Open Babel classes for OBMol and OBConversion
#include <openbabel/mol.h>
#include <openbabel/obconversion.h>

using namespace std;
using namespace OpenBabel;

int main(int argc,char **argv)
{
 int i;
 OBAtom atom;
 // Read from STDIN (cin) and Write to STDOUT (cout)
 OBConversion conv(&cin,&cout);

 // Try to set input format to MDL SD file
 // and output to SMILES
 if(conv.SetInAndOutFormats("MOL2","MOL2"))
 { 
    OBMol mol;
/*
    if(conv.Read(&mol))
    {
       //  ...manipulate molecule 
       cerr << " Molecule has: " << mol.NumAtoms() 
            << " atoms." << endl;

    }
*/
    bool notatend = conv.Read(&mol);
    while (notatend)
    {
      std::cerr << "Molecular Weight: " << mol.GetMolWt() << std::endl;
      
      FOR_ATOMS_OF_MOL(atom, mol) {
//       for (OBMolAtomIter atomi(mol); atomi; atomi++) {
         cerr << atom->GetIdx() << ' ';
         cerr << atom->GetAtomicNum() << ' ';
         cerr << atom->GetVector() << ' ';
	 cerr << atom->GetFormalCharge() << endl;
	 if (atom->GetAtomicNum() == 12)
      	    mol.DeleteAtom(&(*atom), true);
      }
    
      conv.Write(&mol);
      mol.Clear();
      notatend = conv.Read(&mol);
    }
    
 }
 return 0; // exit with success
}
