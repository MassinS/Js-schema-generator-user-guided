
# JSON Schema data generator

Given a [JSON Schema](https://json-schema.org/) S and an integer n, the generator returns : either n instances that are valid against S, UNSAT if the schema does not admit any instance and CANNOT if it fails at generating instances. If it cannot generate n instances, it returns all the instances it was able to generate.


## How to run the generator

- Install sbt
- Clone the project
- Open a terminal and go to the directory of the project
- Run sbt
- Execute the command: run path_to_Schema n

  **Exmaple:** run $HOME/jschemadatagenerator/src/test/expdataset/wp/wp_0_Normalized.json 2


## Authors

- lyes.attouche@dauphine.eu
- mohamed-amine.baazizi@lip6.fr
- dario.colazzo@lamsade.dauphine.fr
    