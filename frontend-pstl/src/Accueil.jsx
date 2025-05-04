import { useState } from 'react'
import brackets from './assets/mdi_code-json.svg'
import { Navigate } from "react-router";
import './App.css'
function Accueil() {
    const [naviguer, setNaviguer] = useState(false);
    return(
        <>
        {naviguer?<Navigate to="/generation"  replace={true} />:null}
        <div className='flex flex-col h-screen w-full'>
              <header className='flex h-1/6 py-3 justify-between items-center'>
              <div className='flex ml-16 mb-3 h-[48px]  items-center'>
              <img src={brackets} alt="Logo" />
              <p className='text-3xl text-white pl-4 font-bold'>JSON Schema</p>
              </div>
              <div className='flex mr-12 h-[48px] items-center'>
                
              </div>
        
              </header>
              <main className='flex h-5/6 w-full justify-center'>
              <div className='flex mt-16 flex-col w-[600px]'>
                <p className='text-5xl font-bold text-[#A2A3FF] '>Pourquoi Schema JSON?</p>
                <p className='mt-4 text-2xl text-white font-normal'>JSON Schema est un outil puissant qui vous permet de définir la structure et les types de données utilisés par vos applications. Il sert de contrat qui précise à quoi doivent ressembler vos données, garantissant ainsi la cohérence et la fiabilité de l'ensemble</p>
                <div className='flex justify-center'>
                <button className='mt-8 py-2 text-xl w-48 text-white rounded-full border-2 border-[#A2A3FF] bg-[#A2A3FF] transition-all duration-300 hover:bg-[#898AD8] hover:shadow-md hover:scale-110' onClick={()=>{
                    setNaviguer(!naviguer)
                }}>
                 commencer
               </button>
                </div>
              </div>
              </main>
           </div>
        </>
        
    )
}
export default Accueil