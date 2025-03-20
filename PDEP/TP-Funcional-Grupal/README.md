# Consigna

Vamos a crear nuestro propio sistema de control de versiones, que permite administrar los sucesivos cambios que los usuarios hacen sobre sus archivos.

## Primera parte

Archivos y carpetas
Los archivos sobre los que se trabaja constan de un nombre que los identifica y un contenido que se puede representar mediante una cadena de caracteres. Los archivos están agrupados en una misma carpeta de la que también se conoce su nombre. No puede haber dos archivos llamados igual en una misma carpeta. 

Se pide:
1. Averiguar si una carpeta contiene un archivo con un nombre dado.

Commits
Un commit da unidad a una serie de tareas que se realizan sucesivamente sobre una carpeta: tiene una descripción y el conjunto de cambios a realizar. Los cambios existentes son los siguientes, aunque en el futuro podría haber otros: 
Crear: hace que aparezca el archivo con el nombre indicado y vacío de contenido. Si ya existe un archivo con ese nombre en la carpeta, no se puede realizar este cambio.
Eliminar: hace que desaparezca el archivo de la carpeta. 
Vaciar: hace que se eliminen todos los archivos de la carpeta
Agregar: implica agregar el texto indicado al inicio del contenido del archivo.
Sacar: implica sacar del contenido del archivo todos caracteres comprendidos entre dos posiciones numéricas dadas.
Salvo “Crear”, los demás cambios no pueden realizarse en caso que no exista en la carpeta ningún archivo                                                                           cuyo nombre sea el indicado. Cuando un cambio no pueda realizarse queda todo igual.

Se pide:
1. Realizar un determinado cambio en una carpeta, en caso que se pueda realizar.
2. Dada una carpeta, efectuar todos los cambios de un commit que se puedan realizar, lo que implica obtener cómo queda dicha carpeta. Por ejemplo: En una carpeta vacía llamada “pdep” se aplican los 3 cambios de un commit con descripcion "commit inicial”: uno que crea el archivo “leeme.md”, otro que crea el archivo “parcial.hs” y otro que agrega “Este es un TP” al archivo “leeme.md”. Finalmente, la carpeta debe quedar con 2 archivos: “leeme.md” con el contenido “Este es un TP” y el archivo “parcial.hs” vacío.
3. Determinar si un commit es inutil para una carpeta, lo cual se deduce de que la carpeta quede igual que antes luego de "cometerlo". 
4. Determinar si un commit que resulta inutil para una carpeta, dejaría de serlo si se realizaran sus cambios en la misma carpeta en orden inverso.  


# Responsables

Jerónimo Fermepin  
Nicolas Bustos  
Lourdes Mendez  
Santiago Apicella  
Alex Tolaba
