import schema from './assets/logos_json-schema-icon.svg'
import brackets from './assets/mdi_code-json.svg'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { faArrowRightFromBracket } from '@fortawesome/free-solid-svg-icons'
import { faAngleDown } from '@fortawesome/free-solid-svg-icons'
import  {useState,useEffect } from 'react';
import { Navigate } from "react-router";
import './App.css'
import './main.css'
import Editor from '@monaco-editor/react';
import axios from "axios";
import toast from 'react-hot-toast'
function Test(){
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
    const [checkStates, setCheck] = useState(checkinit);
    const [Nombre, setNombre] = useState(Nombreinit);
    const personnes={
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



      const handleClick = (index) => {
        // Mettre à jour l'état de l'élément individuel lorsqu'il est cliqué
        const updatedClickedStates = clickedStates.map(() => false);
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

      const renderSchema = (schema, key = "root", parentSelected = true) => {
        // Si le schéma n'est pas un objet valide, on arrête la récursion
        if (!schema || typeof schema !== "object") return null;
      
        return (
          <div key={key} className="ml-4">
            {/* Affichage du nom de la propriété + Checkbox */}
            <div className="flex items-center">
              {parentSelected && (
                <input
                  type="checkbox"
                  className="mr-2"
                  checked={schema.selected || false}
                  onChange={() => handlecheck(key)} // Change l'état de sélection
                />
              )}
              <strong>{key} :</strong>
              <pre className="text-white p-2 rounded">
                {JSON.stringify({ type: schema.type }, null, 2)} {/* On affiche juste le type */}
              </pre>
            </div>
      
            {/* Affichage des sous-éléments récursivement */}
            <div className="ml-6">
              {Object.entries(schema)
                .filter(([subKey]) => !["type", "selected", "instances"].includes(subKey))
                .map(([subKey, subSchema]) => {
                  if (Array.isArray(subSchema)) {
                    return (
                      <div key={subKey} className="ml-4">
                        <strong>{subKey} :</strong>
                        <div className="ml-4">
                          {subSchema.map((item, index) => (
                            
                            <div key={`item-${index}`} className="flex items-center">
                              <pre className="text-white p-2 rounded">
                                {JSON.stringify({ type: item.type }, null, 2)}
                              </pre>
                              <input
                                type="checkbox"
                                className="mr-2"
                                checked={item.selected || false}
                                onChange={() => handlecheck(`${key}-${subKey}-${index}`)} // change l'etat de selection de l'element du tableau
                              />
                            </div>
                          ))}
                        </div>
                      </div>
                    );
                  } else if (subSchema.type == "object" ) {
                    return renderSchema(subSchema, subKey, schema.selected);
                  }
                })}
            </div>
          </div>
        );
      };
      
      
      
      
      
      
      
      
      
      const handlecheck = (key, subIndex = null) => {
        setResponse1((prevState) => {
          console.log("Avant mise à jour de l'état:", prevState);
      
          const updatedBranches = { ...prevState.Branches[0] }; // Clonage de l'objet principal
      
          if (subIndex !== null && Array.isArray(updatedBranches[key])) {
            // Si le key est un tableau (donc une liste de sous-schémas), on modifie le selected du sous-schéma
            updatedBranches[key][subIndex].selected =
              !updatedBranches[key][subIndex].selected;
          } else if (updatedBranches[key]) {
            // Si ce n'est pas un tableau (c'est une branche principale), on modifie le selected de la branche
            updatedBranches[key].selected = !updatedBranches[key].selected;
          }
      
          console.log("Après mise à jour de l'état:", updatedBranches);
      
          return { Branches: [updatedBranches] }; // Retourne l'état mis à jour
        });
      };
      
      
      
      const handleNombre = (key, subIndex, value) => {
        setResponse1((prevState) => {
          const updatedBranches = { ...prevState.Branches[0] };
      
          if (subIndex !== null && updatedBranches[key].anyOf) {
            updatedBranches[key].anyOf[subIndex].instances = value;
          } else {
            updatedBranches[key].instances = value;
          }
      
          return { Branches: [updatedBranches] };
        });
      };
      



      const handleGenerate = () => {
        if (!appuyer1) {
            const obj = { Branches: [] };
            setLoading(true);
            axios.post("http://localhost:9000/renvoieSchemaCanoniser", personnes)
              .then((respo) => {
                const processBranch = (branch) => {
                  // Créer l'objet de base avec type, selected et instances
                  const processedBranch = {
                    type: branch.type,
                    selected: false,
                    instances: 0,
                  };
                
                  // Si la branche a des `patternProperties`, on les transforme en un tableau de types
                  if (branch.patternProperties) {
                    Object.entries(branch.patternProperties).forEach(([key, value]) => {
                      processedBranch[key] = processBranch(value); // Appel récursif pour chaque sous-schéma
                    });
                  }
                
                  // Si la branche a un `anyOf`, on remplace `anyOf` par un tableau de sous-schémas, sans créer la clé `anyOf`
                  if (branch.anyOf) {
                    // Au lieu de créer "anyOf", on transforme directement en tableau de sous-schémas
                    return branch.anyOf.map((subSchema) => processBranch(subSchema)); // Retourne directement un tableau de sous-schémas
                  }
                
                  // Si la branche ne contient ni `anyOf` ni `patternProperties`, on retourne directement le schéma
                  return processedBranch;
                };
                
                
                  
            
                if (respo.data.anyOf) {
                  obj.Branches = respo.data.anyOf.map(processBranch);
                } else {
                  obj.Branches.push(processBranch(respo.data));
                }
          console.log(obj)
          setAppuyer1(true);
          setResponse1(obj);
          handleClick(0);
          }).catch(err => { console.log(err); }).finally(() => {
            setLoading(false);
          }
          );
          
        } else {
          setLoading(true); // Activation du loading avant la boucle

  const promises = response1.Branches.map((branche, index) => {
  if (checkStates[index]) {
   console.log(branche);
    return axios.post('http://localhost:9000/', { schema: branche, count: Nombre[index] })
      .then((respo) => {
        console.log(respo);
        const Schema_type = respo.data;
        const objet = { instances: {} };
        // La première propriété du Schema_type est toujours le générateur
        const generatorType = Object.keys(Schema_type)[0]; // On récupère le nom de la première propriété
        const handleGenerator = (generatorType) => {
          if (Schema_type[generatorType]?.Instances) {
            const instances = Schema_type[generatorType].Instances;
            instances.forEach((instance, ind) => {
              if (!objet.instances[`Branche${index + 1}`]) {
                objet.instances[`Branche${index + 1}`] = {};
              }
              objet.instances[`Branche${index + 1}`][`instance${ind + 1}`] = instance;
            });
        
            // Gérer le toast en fonction du générateur
            if (Schema_type[generatorType]?.Note) {
              toast.success(Schema_type[generatorType].Note); // Note spécifique du générateur
            } else {
              toast.success(`Génération avec succès pour la branche ${index + 1}`); // Message par défaut
            }
          } else {
            toast.error(`${generatorType} pour la branche ${index + 1}`); // Erreur si pas d'instances
          }
        };
        
        handleGenerator(generatorType);
        setAll((prevAll) => ({
          instances: {
            ...prevAll.instances,
            ...objet.instances
          }
        }));
        /*
        setAllINstances((prevAll) => ({
          instances: {
            ...prevAll.instances,
            ...objet.instances
          }
        }));*/
        
      })
      .catch(err => {
        console.log(err);
        toast.error("Cet Objet ne correspond pas à un Schema JSON");
      });
  }
});

// Attendre que toutes les requêtes soient terminées
Promise.all(promises).finally(() => {
  setLoading(false);
});

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
             <ul className='list -bottom-28' style={{display:reverse?'block':'none'}}>
                <li onClick={()=>{
                    Setutilisateur("simple");
                    setAppuyer1(false);
                    setResponse1({Branches: [] });
                    setResponse({instances: []});
                    setAll({instances: []});
                    setCheck(new Array(2).fill(false));
                }} style={{borderLeft:utilisateur=='simple'?'3px solid #898AD8':null}}>mode simple</li>
                <li onClick={()=>{
                    Setutilisateur("expert");
                    setAppuyer1(false);
                    setResponse1({Branches: [] });
                    setResponse({instances: []});
                    setAll({instances: []});
                    setCheck(new Array(2).fill(false));
                }} style={{borderLeft:utilisateur=='expert'?'3px solid #898AD8':null}}>mode expert</li>
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
             

             <div className='flex w-full justify-center mt-8'>
                    <button className='w-48 py-1 text-white rounded-full border-2 border-[#A2A3FF] transition-all duration-300 hover:bg-[#898AD8] hover:shadow-md hover:scale-110' 
                    onClick={()=>{
                      if(typeof Schema ==='object'){
                        setLoading(true);
                        axios.post('http://localhost:9000/',personnes).then((respo)=> {
                          console.log(respo);
                           const Schema_type=respo.data;
                           const objet = { instances: {} };
                          if("NumberGenerator" in Schema_type){
                            const a=Schema_type.NumberGenerator.Instances;
                            a.forEach((instance, index) => {
                              objet.instances[`instance${index + 1}`] = instance;
                              });
                            setResponse(objet);
                          }else if("StringGenerator" in Schema_type){
                            const a=Schema_type.StringGenerator.Instances;
                            a.forEach((instance, index) => {
                              objet.instances[`instance${index + 1}`] = instance;
                              });
                            setResponse(objet);
                          }else if("BooleanGenerator" in Schema_type){
                            const a=Schema_type.BooleanGenerator.Instances;
                            a.forEach((instance, index) => {
                              objet.instances[`instance${index + 1}`] = instance;
                              });
                            setResponse(objet);
                          }else if("SchemaGenerator" in Schema_type){
                            const a=Schema_type.SchemaGenerator.Instances;
                            a.forEach((instance, index) => {
                              objet.instances[`instance${index + 1}`] = instance;
                              });
                            setResponse(objet);
                          }
                          if("SchemaGenerator" in Schema_type && "Note" in Schema_type.SchemaGenerator){
                            toast.success(Schema_type.SchemaGenerator.Note);
                          }else{
                            toast.success("generation avec succes");
                          }
                          
                          
                      })
                  .catch(err=>{
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
             <div>
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
                   }} />:
                   <div className="input h-[416px] px-2 py-2 mt-4 w-xl bg-[#1E293B] text-[#A2A3FF] overflow-auto">
  {response1.Branches.length > 0 ? (
      <div>
        {response1.Branches.map((branche, index) => renderSchema(branche, index,false))}
      </div>
    ) : null}





</div>
}
                   
                  

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
      handleClick(index);
      setAll({instances: []});
      setCheck(new Array(response1.Branches ?Object.keys(response1.Branches).length:20).fill(false));
    }} style={getPageStyle(index)} >{index+1} </div>
  ))}
</div>:null}
             <div className='flex w-full justify-center mt-8'>

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
export default Test