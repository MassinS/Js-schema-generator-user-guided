import schema from './assets/logos_json-schema-icon.svg'
import brackets from './assets/mdi_code-json.svg'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faArrowRightFromBracket } from '@fortawesome/free-solid-svg-icons'
import { faAngleDown } from '@fortawesome/free-solid-svg-icons'
import { faX } from '@fortawesome/free-solid-svg-icons'
import  {useState,useEffect } from 'react';
import { Navigate } from "react-router";
import './App.css'
import './main.css'
import Editor from '@monaco-editor/react';
import axios from "axios";
import toast from 'react-hot-toast'
import Ajv from 'ajv'
import Ajv2019 from "ajv/dist/2019";
import Ajv2020 from "ajv/dist/2020";
function Generation(){
    const [naviguer, setNaviguer] = useState(false);
    const [nbr_instance, setNbr] = useState(1);
    const[response,setResponse] =useState({ instances: [] });
    const [response1, setResponse1] = useState({ 
      Branches: [
        
      ] 
    });
    const[Schema,setSchema]=useState({});
    const[All,setAll]=useState({ instances: [] });
    const [AllInstances,setAllINstances]=useState({ instances: [] });
    const [loading, setLoading] = useState(false);
    const[indexPage,Setpage]=useState(0);
    const[utilisateur,Setutilisateur]=useState("simple");
    const [reverse,setReverse]=useState(false);
    const [appuyer1,setAppuyer1]=useState(false);
    const nbrPages = response1.Branches ? Math.ceil(Object.keys(response1.Branches).length / 2) : 0;
    const initialClickedStates = new Array(nbrPages).fill(false);
    const checkinit = new Array( response1.Branches ?Object.keys(response1.Branches).length:2).fill(false);// a discuter
    const Nombreinit = new Array(2).fill(1);
    const [clickedStates, setClickedStates] = useState(initialClickedStates);
    const SchemaNBInstance={
      schema:Schema,
      count:nbr_instance
    };

    const handleEditorDidMount = (editor, monaco) => {
        monaco.editor.defineTheme("myCustomTheme", {
          base: "vs-dark", 
          inherit: true,
          rules: [
            { token: "", background: "#1E293B", foreground: "ffffff" },
            { token: "comment", foreground: "7e7e7e", fontStyle: "italic" },
            { token: "keyword", foreground: "ff6188" },
            { token: "number", foreground: "fd971f" },
            { token: "string", foreground: "a9dc76" },
          ],
          colors: {
            "editor.background": "#1E293B",
            "editor.foreground": "#ffffff",
            "editor.lineHighlightBackground": "#1E293B",
            "editorGutter.background": "#1E293B",
            "editorGutter.foreground": "#858585",
            "editorCursor.foreground": "#ffcc00",
            "editor.selectionBackground": "#1E293B",
          },
        });
        monaco.editor.setTheme("myCustomTheme");
      };
    /*useEffect(() => {
        console.log(JSON.stringify(response1,null,2));
      },[response1]);*/

useEffect(() => {
  const schema = All.instances;
  const instance = schema.Branche1?.[0] || schema.anyof?.[0]?.Branche1;
  if (!instance) return;

  // On détecte le draft à partir de $schema
  const schemaVersion = Schema?.$schema || "http://json-schema.org/draft-07/schema#";
  let ajv;
  if (schemaVersion.includes("2020")) {
    ajv = new Ajv2020({ allErrors: true });
  } else if (schemaVersion.includes("2019")) {
    ajv = new Ajv2019({ allErrors: true });
  } else {
    ajv = new Ajv({ allErrors: true }); // Draft 7 par défaut
  }

  try {
    const validate = ajv.compile(Schema);
    const valid = validate(instance);

    if (!valid) {
      console.log("Erreurs de validation :", validate.errors);
    } else {
      console.log("Instance valide !");
    }
  } catch (e) {
    console.error("Erreur de compilation du schéma :", e);
  }
}, [All]);
   
    


      const handleClick = (index) => {
        // Mettre à jour l'état de l'élément individuel lorsqu'il est cliqué
        const updatedClickedStates = clickedStates.map(()=>false);
        updatedClickedStates[index] = true;
        setClickedStates(updatedClickedStates);
      };
      
      const getPageStyle = (index) => {
        return {
          backgroundColor: clickedStates[index] ? '#898AD8' : null,
          color: clickedStates[index] ?'#fff': null,
          cursor: 'pointer',
        };
      };
   

const renderSchema = (schema, key = "root", parentSelected, indent, path) => {
  if (!schema || typeof schema !== "object") return null;

  const currentPath = key.startsWith("Branche") && path.length > 0
  ? path // ici je garde path car j'affiche juste le nom de la branche mais moi je manipule des indices
  : [...path, key]; // dans d'autre cas oui on descent

  // Fonction pour vérifier si un enfant a la propriété selected et il descend dans le cas d'un anyOf
  const hasSelectedChild = (schema) => {
    return Object.values(schema).some((child) => {
      if (typeof child !== "object" || child === null) return false;
  
      // cas normal
      if (child.selected) return true;
  
      // cas special, si c'est un anyOf, on descend dans les enfants
      if (Array.isArray(child)) {
        return child.some(
          (sub) => typeof sub === "object" && sub !== null && sub.selected
        );
      }
  
      return false;
    });
  };
  

  return (
    <div key={key} className="ml-2 mt-1" style={{ marginLeft: `${indent * 2}px` }}>
      <div className="flex items-center">
        {(indent === 0 || parentSelected) && (
          <input
            type="checkbox"
            className="mr-2"
            checked={schema.selected || false}
            onChange={() => {
              handlecheck(key, currentPath);
            }}
          />
        )}
        <strong>{key} :</strong>
        <span className="text[#A2A3FF]">{" {"}</span>
      </div>

      {!hasSelectedChild(schema) && schema.selected && (
        <div className="mt-1">
          <input
            type="text"
            className="bg-gray-700 rounded-full text-white text-center border border-gray-500 px-2 py-1 w-24"
            value={schema.instances || 1}
            onChange={(e) => handleNombre(key, currentPath, Number(e.target.value))}
          />
        </div>
      )}

      {Object.entries(schema).map(([subKey, subValue]) => {
  
        // Traitement spécial pour anyOf
        if (Array.isArray(subValue) && subKey === "anyOf") {
          return (
            <div key={subKey} className="ml-4">
              <strong className="text-[#A2A3FF]">
                anyOf:
              </strong>
              {subValue.map((option, index) => (
                <div key={index} className="ml-4">
                  {renderSchema(
                    option,
                    `Branche${index + 1}`, 
                    schema.selected, 
                    indent + 2,
                    [...currentPath, "anyOf", index] 
                  )}
                </div>
              ))}
            </div>
          );
        }
        if (["selected", "instances"].includes(subKey)) {
          return null;
        }
        

        return (
          <div key={subKey} className="ml-4">
            {(typeof subValue !== "object" || Array.isArray(subValue)) &&
            !(subKey === "type" && subValue === undefined) ? (
              <strong>{subKey}:</strong>
            ) : null}

            {Array.isArray(subValue) ? (
              <pre className="text-[#A2A3FF]">
                {JSON.stringify(
                  subValue.map((item) =>
                    typeof item === "object" && !Array.isArray(item) && item !== null
                      ? Object.fromEntries(
                          Object.entries(item).filter(([k]) => !["selected", "instances"].includes(k))
                        )
                      : item
                  ),
                  null,
                  2
                )}
              </pre>
            ) : typeof subValue === "object" ? (
              <div className="ml-4">
                {renderSchema(subValue, subKey, schema.selected, indent + 1, currentPath)}
              </div>
            ) : (
              <span className="text[#A2A3FF]"> {subValue}</span>
            )}
          </div>
        );
      })}

      <div className="p-2 rounded bg-gray-800">
        <span className="text[#A2A3FF]">{" }"}</span>
        {schema.correspondance && (
          <div className="text-red-500">
            <FontAwesomeIcon icon={faX} className="mr-1" />
            {schema.correspondance}
          </div>
        )}
      </div>
    </div>
  );
};

 
      const handlecheck = (key, path = []) => {
        setResponse1((prevState) => {
          if (!prevState.Branches) {
            console.error("Branches n'est pas defini !");
            return prevState; // si l'etat ne contient pas Branches alors on retourne l'etat tel qu'elle est
          }
      
          // Copie de l'objet Branches
          const updatedBranches = { ...prevState.Branches };
      
          // fonction récursive pour accéder à une propriete spécifique
          const updateSelection = (obj, path) => {
            if (path.length === 0) return obj;
          
            const [currentKey, ...restPath] = path;
            const updatedObj = Array.isArray(obj) ? [...obj] : { ...obj }; //
          
            if (updatedObj[currentKey] !== undefined) {
              if (restPath.length === 0) {
                // mettre a jour le selected de la cle cible
                updatedObj[currentKey] = {
                  ...updatedObj[currentKey],
                  selected: !updatedObj[currentKey].selected,
                };
              } else {
                //descendre recursivement
                updatedObj[currentKey] = updateSelection(updatedObj[currentKey], restPath);
              }
            }
          
            return updatedObj;
          };
          
          // utilisation de la fonction recursive pour mettre à jour la sélection avec le chemin complet
          const updatedObj = updateSelection(updatedBranches, path);
          // remise à jour de l'etat
          return { Branches: updatedObj }; 
        });
      };

      const updateInstances = (obj, path,value) => {
        if (path.length === 0) return obj;
      
        const [currentKey, ...restPath] = path;
        const updatedObj = Array.isArray(obj) ? [...obj] : { ...obj };
      
        if (updatedObj[currentKey] !== undefined) {
          if (restPath.length === 0) {
            updatedObj[currentKey] = {
              ...updatedObj[currentKey],
              instances: value,
            };
            console.log(updatedObj[currentKey]);
          } else {
            updatedObj[currentKey] = updateInstances(updatedObj[currentKey], restPath,value);
          }
        }
      
        return updatedObj;
      };
      const handleNombre = (key, path, value) => {
        setResponse1((prevState) => {
          if (!prevState.Branches) return prevState;
      
          const updatedBranches = updateInstances(prevState.Branches, path,value);
          console.log(value);
          return { Branches: updatedBranches };
        });
      };
      
    
      



      const handleGenerate = () => {
        if (!appuyer1) {
            const obj = { Branches: [] };
            setLoading(true);
            axios.post("http://localhost:9000/renvoieSchemaCanoniser", SchemaNBInstance)
              .then((respo) => {
                
                const processBranch = (branch, index = 1, isRootAnyOf = false) => {
                  if (typeof branch !== "object" || branch === null) return branch;
                
                  const processedBranch = Array.isArray(branch) ? [...branch] : { ...branch };
                
                  processedBranch.selected = false;
                  processedBranch.instances = 0;
                
                  // Si la racine est un anyof ,diviser en plusieurs branches
                  if (isRootAnyOf && Array.isArray(branch)) {
                    const branches = {};
                    branch.forEach((subSchema, idx) => {
                      branches[`Branche${idx + 1}`] = processBranch(subSchema, idx + 1);
                    });
                    return branches;
                  }
                
                  // si c'est un anyOf à l'interieur (pas la racine), on le laisse tel quel
                  if (Array.isArray(processedBranch.anyOf)) {
                    processedBranch.anyOf = processedBranch.anyOf.map((subSchema) =>
                      processBranch(subSchema)
                    );
                  }
                
                  // traitement recursif des autres proprietes
                  for (const key in processedBranch) {
                    if (
                      key !== "anyOf" &&
                      typeof processedBranch[key] === "object" &&
                      processedBranch[key] !== null
                    ) {
                      processedBranch[key] = processBranch(processedBranch[key]);
                    }
                  }
                
                  return processedBranch;
                };
                
                
                
                if (respo.data.anyOf) {
                  obj.Branches = {};
                  respo.data.anyOf.forEach((branch, index) => {
                    obj.Branches[`branch${index + 1}`] = processBranch(branch);
                  });
                } else {
                  obj.Branches["Branche1"] = processBranch(respo.data);
                }                    
          setAppuyer1(true);
          setResponse1(obj);
          handleClick(0);
          }).catch(err => { console.log(err); }).finally(() => {
            setLoading(false);
          }
          );
          
        } else {
          setLoading(true); // activer le chargement

          const cartesianProduct = (...arrays) => {
            const result = [];
          
            const recursiveCombine = (arrays, index = 0, current = {}) => {
              if (index === arrays.length) {
                result.push({ ...current });
                return;
              }
          
              for (let i = 0; i < arrays[index].length; i++) {
                const currentObject = arrays[index][i];
          
                // ajouter chaque cle et sa valeur a la combinaison
                Object.keys(currentObject).forEach(key => {
                  current[key] = currentObject[key];
          
                  recursiveCombine(arrays, index + 1, current);
          
                  delete current[key]; // supprimer la cle pour la prochaine itération
                });
              }
            };
          
            recursiveCombine(arrays);
          
            return result;
          };

          const processSchema = async (branche, path, Key) => {
            if (!branche || !branche.selected) return null;
          
            if (branche.type && branche.type !== "object") {
              // genération standard
              return axios.post("http://localhost:9000/", { schema: branche, count: branche.instances || 1 })
                .then(res => {
                  const Schema_type = res.data;
                  const generatorType = Object.keys(Schema_type)[0];
                  const generatedInstances = Schema_type[generatorType]?.Instances || [];
                  if (generatedInstances.length > 0) {
                    return generatedInstances;
                  } else {
                    toast.error(Key + " est insatisfiable");
                  }
                })
                .catch(err => {
                  console.error("Erreur lors de la génération :", err);
                  return [];
                });
            }
          
            let sousSchemas = {};
          
            // traitement special de anyOf
            if (Array.isArray(branche.anyOf)) {
              const options = await Promise.all(
                branche.anyOf.map((subSchema, idx) =>
                  processSchema(subSchema, [...path, "anyOf", idx], `${Key} (Branche${idx + 1})`)
                )
              );
          
              // On garde seulement les résultats valides
              sousSchemas["anyOf"] = options.filter(opt => opt !== null);
            }
          
            await Promise.all(
              Object.entries(branche).map(async ([key, value]) => {
                if (["type", "selected", "instances", "anyOf"].includes(key)) return;
            
                if (key === "patternProperties") {
                  //cas special pour patternProperties
                  await Promise.all(
                    Object.entries(value).map(async ([patternKey, patternValue]) => {
                      const processed = await processSchema(patternValue, [...path, patternKey], Key);
                      if (processed) sousSchemas[patternKey] = processed;
                    })
                  );
                } else {
                  const processed = await processSchema(value, [...path, key], Key);
                  if (processed) sousSchemas[key] = processed;
                }
              })
            );
            
          
            return sousSchemas;
          };
          



// fonction principale qui declenche la generation
const generateInstances = async () => {
  let allInstances = {};
  await Promise.all(
    Object.entries(response1.Branches).map(async ([key, branche]) => {
      const processedInstances = await processSchema(branche,[],key);
      if (processedInstances) {
        allInstances[key] = processedInstances; // stocker les instances dans la branche qui correspond(branche 1|2|3 ....)
      }
    })
  );

  const transformInstancesForCartesian = (nestedInstances) => {
    let result = [];
  
    if (Array.isArray(nestedInstances)) {
      return nestedInstances.map((instance, index) => ({ [`instance${String(index + 1)}`]: instance }));
    }
  
    let allProps = [];
  
    Object.entries(nestedInstances).forEach(([key, value]) => {
      let instancesArray = [];
  
      if (Array.isArray(value)) {
        // si chaque element est un objet, on retourne chaque objet individuellement
        instancesArray = value.map((instance) => {
          const flat = applatir(instance);
          return Array.isArray(flat)
            ? flat.map(f => ({ [key]: f }))
            : [{ [key]: flat }];
        }).flat();
      } else if (typeof value === "object" && value !== null) {
        // traiter les proprietes imbriquées de manière separe
        const transformedChildren = transformInstancesForCartesian(value);
    
        instancesArray = transformedChildren.flatMap((childInstance) => {
            const flattenedInstances = applatir(childInstance); // Maintenant retourne un tableau
            return flattenedInstances.map(flattened => ({ [key]: flattened }));
        });
    } else {
        instancesArray = [{ [key]: applatir(value) }];
      }
  
      if (instancesArray.length > 0) {
        allProps.push(instancesArray);
      }
    });
  
    // si des proprietes ont ete trouves, on genere le produit cartesien
    if (allProps.length > 0) {
      result = cartesianProduct(...allProps);
      result = Array.from(new Set(result.map(JSON.stringify))).map(JSON.parse); // Suppression finale des doublons
    }
    
  
    return result;
  };


  const applatir = (obj) => {
    if (typeof obj !== "object" || obj === null) {
      return obj; // Type primitif
    }
  
    if (Array.isArray(obj)) {
      return obj.map(applatir).filter(item => item !== undefined);
    }
  
    let newObj = [];
  
    Object.entries(obj).forEach(([key, value]) => {
      let flatValue = applatir(value);
      // Vérifie si c'est une clé "anyOf" ou "BrancheX"
      if (key.toLowerCase() === "anyof" || /^Branche\d+$/i.test(key) || key.toLowerCase() === "patternproperties") {
        if (Array.isArray(flatValue)) {
          flatValue.forEach(item => {
            newObj.push(item); // Injecter directement sans wrapper avec la clé
          });
        } else {
          newObj.push(flatValue); // Injecter directement
        }
      } else {
        if (Array.isArray(flatValue)) {
          flatValue.forEach(item => {
            newObj.push({ [key]: item });
          });
        } else {
          newObj.push({ [key]: flatValue });
        }
      }
    });
  
    return newObj;
  };
  
  



  
  // Traitement des branches
  Object.entries(allInstances).forEach(([brancheName, nestedInstances]) => {  
      setAll((prevAll) => ({
        ...prevAll,
        instances: {
          ...prevAll.instances,
          [brancheName]: transformInstancesForCartesian(nestedInstances)
        }
      }));
    
  });
  setLoading(false);
};
generateInstances();
        }
      };
  
    return(
        <>
        {naviguer?<Navigate to="/"  replace={true} />:null}
        {loading && (
        
        <div className='slow-rotate h-6 w-6 bg-[#A2A3FF]'></div>
       
         )}
        <div className="flex w-full h-full">
            <div className="w-[288px] h-screen">
            <div className=' flex pl-3 pt-8 items-center'>
                <img src={brackets} alt="Logo" />
                <p className='text-white text-3xl font-semibold ml-3'>JSON Schema</p>
            </div>
            <div className='mt-20 parent 
            relative
            flex
            items-center
            py-2 h-14
            rounded-2xl
            pl-3
            item
             '  >
                <img src={schema} alt='erreur' />
                <li className='list-none ml-3 child text-base text-white child'>Schema instance</li>
             </div>
             <div className='flex w-[270px] bottom-4 pl-3 items-center py-2 h-14 hover:bg-white rounded-2xl parent absolute retour' onClick={()=>{
                setNaviguer(!naviguer)
             }}>
              <FontAwesomeIcon icon={faArrowRightFromBracket} className='text-lg w-5 text-white child' />
              <li className='list-none ml-3 text-white text-base child '>Revenir</li>
             </div>
            </div>

            <div className="flex flex-col px-8 h-screen w-[calc(100%-288px)] shadow-lg shadow-white/20">
            
              <div className='flex justify-between w-full mt-10'>
                <p className='text-white text-4xl font-bold'>Générateur de Schema Json</p>
                <div className='drop relative' onClick={()=>{
            setReverse(!reverse);
        }}>
            <span className='text-gray-500'>{utilisateur}</span>
            <FontAwesomeIcon
             icon={faAngleDown}
             className={reverse ? 'icone rotate' : 'icone'} />
             <ul className='list -bottom-40' style={{display:reverse?'block':'none'}}>
                <li onClick={()=>{
                    Setutilisateur("simple");
                    setAppuyer1(false);
                    setResponse1({Branches: [] });
                    setResponse({instances: []});
                    setAll({instances: []});
                }} style={{borderLeft:utilisateur=='simple'?'3px solid #898AD8':null}}>mode simple</li>
                <li onClick={()=>{
                    Setutilisateur("expert");
                    setAppuyer1(false);
                    setResponse1({Branches: [] });
                    setResponse({instances: []});
                    setAll({instances: []});
                }} style={{borderLeft:utilisateur=='expert'?'3px solid #898AD8':null}}>mode expert</li>
                <li onClick={()=>{
                    Setutilisateur("IA");
                    setAppuyer1(false);
                    setResponse1({Branches: [] });
                    setResponse({instances: []});
                    setAll({instances: []});
                }} style={{borderLeft:utilisateur=='IA'?'3px solid #898AD8':null}}>mode IA</li>
             </ul>
        </div>

             </div>
             {utilisateur=="simple"?<div>
             <div className='flex mt-8 justify-between'>
                <div className='flex flex-col'>
                  <div className='flex items-center h-10'>
                  <p className='text-white font-semibold'>JSON Schema</p>
                  </div>
                    
                    
                
                    <Editor height={436} className='monaco-container' theme="myCustomTheme" onMount={handleEditorDidMount} width={600} defaultLanguage="javascript" options={{
                     quickSuggestions: false, // Désactive les suggestions automatiques
                     inlineSuggest: false, // Désactive les suggestions en ligne
                     suggestOnTriggerCharacters: false, // Désactive les suggestions après un caractère spécial
                     hover: false, // Désactive les infos au survol
                     lineNumbersMinChars: 2, // Réduit l'espace des numéros de ligne
                    disableHints: true, // (Optionnel) Empêche les recommandations
                    renderValidationDecorations: "off", // Désactive complètement les traits rouges d'erreur
                    diagnosticsOptions: {
                      noSyntaxValidation: true, // Désactive la validation syntaxique
                      noSemanticValidation: true, // Désactive la validation sémantique
                    }
                   }} onChange={(value)=>{
                    try {
                      const jsonValue = JSON.parse(value); // Convertir la chaîne en JSON
                      setSchema(jsonValue);
                    } catch (error) {
                      console.error("JSON invalide :", error)
                    }
                   }} />
                </div>
                <div className='flex flex-col'>
                  <div className='flex justify-between items-center'>
                  <p className='text-white font-semibold'>Instances JSON</p>
                  <input
                placeholder="nombre d'instance"
                className="text-white rounded-full px-4 py-2 input text-center"
                onChange={(e) => {
                  setNbr(parseInt(e.target.value, 10));
                }}
              />
                  </div>
                    
                    <div className="input h-[416px] px-2 py-2 mt-4 w-xl bg-[#1E293B] text-[#A2A3FF] overflow-auto ">
                    {
    
    Object.keys(response.instances).length>0 ? (
      <div>
        {
          // Créer un objet à partir des instances
          (() => {
            // Affichage de l'objet JSON
            return (
              <pre>{JSON.stringify(response, null, 2)}</pre>
            );
          })()
        }
      </div>
    ) : null
  }
                    </div>
                </div>
             </div>
             

             <div className='flex w-full justify-center mt-8'>
                    <button className='w-48 py-1 text-white rounded-full border-2 border-[#A2A3FF] transition-all duration-300 hover:bg-[#898AD8] hover:shadow-md hover:scale-110' 
                    onClick={()=>{
                      if(typeof Schema ==='object'){
                        setLoading(true);
                        axios.post('http://localhost:9000/', SchemaNBInstance).then((respo) => {
                          const Schema_type = respo.data;
                          const objet = { instances: {} };
                        
                          const generatorKey = Object.keys(Schema_type)[0]; // récupère "NumberGenerator", "StringGenerator", etc.
                          const generatorData = Schema_type[generatorKey];
                        
                          if (generatorData?.Instances) {
                            generatorData.Instances.forEach((instance, index) => {
                              objet.instances[`instance${index + 1}`] = instance;
                            });
                            setResponse(objet);
                          }
                        
                          // Gestion du message de succès
                          if (generatorKey === "SchemaGenerator" && generatorData.Note) {
                            toast.success(generatorData.Note);
                          } else {
                            toast.success("Génération avec succès");
                          }
                        }).catch(err=>{
                    toast.error("Cet Objet ne correspond pas a un Schema JSON");
                  }).finally(()=>{
                    setLoading(false);
                  });
                      }else{
                        toast.error("Cela n'est meme pas un Objet JSON");
                      }
                      
                    }}>generer</button>
                </div>
            </div>:
            utilisateur==='IA'?
            <div>
             <div className='flex mt-8 justify-between'>
                <div className='flex flex-col'>
                  <div className='flex items-center h-10'>
                  <p className='text-white font-semibold'>JSON Schema</p>
                  </div>
                    
                    
                
                    <Editor height={436} className='monaco-container' theme="myCustomTheme" onMount={handleEditorDidMount} width={600} defaultLanguage="javascript" options={{
                     quickSuggestions: false, // Désactive les suggestions automatiques
                     inlineSuggest: false, // Désactive les suggestions en ligne
                     suggestOnTriggerCharacters: false, // Désactive les suggestions après un caractère spécial
                     hover: false, // Désactive les infos au survol
                     lineNumbersMinChars: 2, // Réduit l'espace des numéros de ligne
                    disableHints: true, // Empêche les recommandations
                    renderValidationDecorations: "off", // Désactive complètement les traits rouges d'erreur
                    diagnosticsOptions: {
                      noSyntaxValidation: true, // Désactive la validation syntaxique
                      noSemanticValidation: true, // Désactive la validation sémantique
                    }
                   }} onChange={(value)=>{
                    try {
                      const jsonValue = JSON.parse(value); // Convertir la chaîne en JSON
                      setSchema(jsonValue);
                    } catch (error) {
                      console.error("JSON invalide :", error)
                    }
                   }} />
                  

                </div> 
                <div className='flex flex-col'>
                  <div className='flex justify-between items-center'>
                  <p className='text-white font-semibold'>Instances JSON</p>
                  <input placeholder="nombre d'instance" className='text-white rounded-full px-4 py-2 input text-center' onChange={(e)=>{
                    setNbr(parseInt(e.target.value, 10));
                  }} />
                  </div>
                    
                    <div className="input h-[416px] px-2 py-2 mt-4 w-xl bg-[#1E293B] text-[#A2A3FF] overflow-auto ">
                    {
                    
                    Object.keys(response.instances).length>0 ? (
                      <div>
                        {
                          // Créer un objet à partir des instances
                          (() => {
                            // Affichage de l'objet JSON
                            return (
                              <pre>{JSON.stringify(response, null, 2)}</pre>
                            );
                          })()
                        }
                      </div>
                    ) : null
  }
                    </div>
                </div>
             </div>

             <div className='flex w-full justify-center mt-4'>

                    <button className='w-48 py-1 text-white rounded-full border-2 border-[#A2A3FF] transition-all duration-300 hover:bg-[#898AD8] hover:shadow-md hover:scale-110' 
                    onClick={()=>{
                      if(typeof Schema ==='object'){
                        setLoading(true);
                        const SchemaG={schema:Schema,numInstances:nbr_instance}
                        axios.post('http://localhost:9000/api/generate', SchemaG).then((respo) => {
                          const objet = { instances: {} };
                          const generatorData = respo.data;
                          console.log(generatorData);
                        
                          if (generatorData) {
                            Object.entries(generatorData).forEach(([_, value], index) => {
                              objet.instances[`instance${index + 1}`] = value;
                            });
                            setResponse(objet);
                          }
                          toast.success("Génération avec succès");
                                                 
                        }).catch(err=>{
                    toast.error("Cet Objet ne correspond pas a un Schema JSON");
                    console.log(err)
                  }).finally(()=>{
                    setLoading(false);
                  });
                      }else{
                        toast.error("Cela n'est meme pas un Objet JSON");
                      }
                      
                      }} >generer</button>
                </div>
            </div>
            
            
            :<div>
             <div className='flex mt-8 justify-between'>
                <div className='flex flex-col'>
                  <div className='flex items-center h-10'>
                  <p className='text-white font-semibold'>JSON Schema</p>
                  </div>
                    
                    
                
                    {!appuyer1?<Editor height={436} className='monaco-container' theme="myCustomTheme" onMount={handleEditorDidMount} width={600} defaultLanguage="javascript" options={{
                     quickSuggestions: false, // Désactive les suggestions automatiques
                     inlineSuggest: false, // Désactive les suggestions en ligne
                     suggestOnTriggerCharacters: false, // Désactive les suggestions après un caractère spécial
                     hover: false, // Désactive les infos au survol
                     lineNumbersMinChars: 2, // Réduit l'espace des numéros de ligne
                    disableHints: true, // Empêche les recommandations
                    renderValidationDecorations: "off", // Désactive complètement les traits rouges d'erreur
                    diagnosticsOptions: {
                      noSyntaxValidation: true, // Désactive la validation syntaxique
                      noSemanticValidation: true, // Désactive la validation sémantique
                    }
                   }} onChange={(value)=>{
                    try {
                      const jsonValue = JSON.parse(value); // Convertir la chaîne en JSON
                      setSchema(jsonValue);
                    } catch (error) {
                      console.error("JSON invalide :", error)
                    }
                   }} />:
                   <div className="input h-[416px] px-2 py-2 mt-4 w-xl bg-[#1E293B] text-[#A2A3FF] overflow-auto">
                  {Object.keys(response1.Branches).length > 0 ? (
                      <div>
                        {Object.entries(response1.Branches).map(([key, branche], index) => {
                    // if (index >= indexPage && index < Math.min(indexPage + 2, Object.entries(response1.Branches).length)) {
                          return renderSchema(branche, key, true, 0,[]);
                      //}
                  })}
                      </div>
                      ) : null}
                  </div>
                  }
                   
                  

                </div> 
                <div className='flex flex-col'>
                  <div className='flex justify-between items-center'>
                  <p className='text-white font-semibold'>Instances JSON</p>
                  <input placeholder="nombre d'instance" className='text-white rounded-full px-4 py-2 input text-center' style={{ visibility: 'hidden' }} onChange={(e)=>{
                    setNbr(parseInt(e.target.value, 10));
                  }} />
                  </div>
                    
                    <div className="input h-[416px] px-2 py-2 mt-4 w-xl bg-[#1E293B] text-[#A2A3FF] overflow-auto ">
                    {
                    
    Object.keys(All.instances).length>0 ? (
      <div>
        {
          // Créer un objet à partir des instances
          (() => {
            // Affichage de l'objet JSON
            return (
              <pre>{JSON.stringify(All, null, 2)}</pre>
            );
          })()
        }
      </div>
    ) : null
  }
                    </div>
                </div>
             </div>
             {Object.keys(response1.Branches).length>0?<div className='flex w-full justify-center mt-6 '>
  {Array.from({ length: nbrPages }, (_, index) => (
    <div key={index} className='pages' onClick={()=>{
      Setpage(index*2);
      setAll({instances: []});
      handleClick(index);
      Object.values(response1.Branches).forEach((branch) => {
        branch.selected = false;
    });

    }} style={getPageStyle(index)} >{index+1} </div>
  ))}
</div>:null}
             <div className='flex w-full justify-center mt-4'>

                    <button className='w-48 py-1 text-white rounded-full border-2 border-[#A2A3FF] transition-all duration-300 hover:bg-[#898AD8] hover:shadow-md hover:scale-110' 
                    onClick={()=>{
                      handleGenerate()}} >generer</button>
                </div>
            </div>}
             
            </div>


        </div>
        </>
        
    )
}
export default Generation;
