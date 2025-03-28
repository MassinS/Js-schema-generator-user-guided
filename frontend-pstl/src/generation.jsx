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
      const handlecheck = (index) => {
        setCheck(prev => {
          const updated = [...prev];
          updated[index] = !updated[index]; // Toggle entre true/false
          return updated;
        });
      };
      
      const handleNombre = (index, value) => {
        const updatedNombre = [...Nombre]; // Créer une copie
        updatedNombre[index] = value; // Modifier sans muter l'original
        setNombre(updatedNombre);
      };
      const getPageStyle = (index) => {
        return {
          backgroundColor: clickedStates[index] ? '#898AD8' : null,
          color: clickedStates[index] ?'#fff': null,
          cursor: 'pointer',
        };
      };
      const handleGenerate = () => {
        if (!appuyer1) {
          const obj = {
            Branches: [
              
            ]
          };
          setLoading(true);
          axios.post('http://localhost:9000/renvoieSchemaCanoniser', personnes).then((respo) => {
          if(respo.data.anyOf){
            respo.data.anyOf.forEach((element, index) => {
              obj.Branches.push(element);
            });
          }else{
            obj.Branches.push(respo.data);
          }
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
              toast.success(`Generation avec succès pour la branche ${index + 1}`); // Message par défaut
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
  {Object.keys(response1.Branches).length > 0 ? (
    <div>
      Branches: [
      {response1.Branches.map((branche, index) => {
        if (index >= indexPage && index < Math.min(indexPage + 2, response1.Branches.length)) {
          return (
            <div key={index}>
              <pre className="mb-2">
                <input
                  type="checkbox"
                  checked={checkStates[index]}
                  id={`branche${index + 1}`}
                  className="mr-4"
                  onClick={() => {
                    handlecheck(index);
                  }}
                />
                Branche{index + 1}:{JSON.stringify(branche, null, 2)}
                {checkStates[index] ? (
                  <input
                  placeholder="nombre d'instance"
                  
                  className="text-white rounded-full px-2 py-1 input text-center w-fit ml-1"
                  onChange={(e) => {
                    handleNombre(index, parseInt(e.target.value, 10));
                  }}
                />
                ) : null}
              </pre>
            </div>
          );
        }
        return null;
      })}
      ]
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
export default Generation