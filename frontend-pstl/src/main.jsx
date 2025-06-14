import { StrictMode } from 'react'
import { createRoot } from 'react-dom/client'
import './index.css'
import { BrowserRouter } from "react-router";
import { Navigate, Route, Routes } from "react-router";
import Accueil from "./Accueil"
import Generation from "./generation";
import { Toaster } from 'react-hot-toast'
createRoot(document.getElementById('root')).render(
  <StrictMode>
    <BrowserRouter>
    <Routes>
      <Route path="/" element={<Accueil />} />
      <Route path="/generation" element={<Generation />}/>
    </Routes>
  </BrowserRouter>
  <Toaster position='top-center'/>
  </StrictMode>,
)
