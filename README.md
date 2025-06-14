# 🧩 Génération de données JSON guidée par l’utilisateur

Ce projet permet à un utilisateur de générer des données JSON à partir d’un schéma, avec une interface graphique côté **frontend** (React), et une logique de traitement côté **backend** (Scala avec SBT et le framework **Play** pour les API).

---

## ✅ Prérequis

- [Node.js](https://nodejs.org/en/) (version recommandée : 18+)
- [Java JDK](https://jdk.java.net/) (**version 20 ou supérieure requise**)
- [Scala](https://www.scala-lang.org/download/)
- [SBT](https://www.scala-sbt.org/download.html) (Scala Build Tool)
- [Visual Studio Code](https://code.visualstudio.com/) avec les extensions :
  - Scala (Metals)
  - SBT (recommandé)


## 🔧 Installation

### 🔹 Frontend

```bash
cd frontend-pstl
npm install
```

### 🔹 Backend

#### 1. 🛠️ Installer SBT  
Suivre les instructions sur :  
👉 [https://www.scala-sbt.org/download.html](https://www.scala-sbt.org/download.html)

---

#### 2. 🧩 Installer l’extension Scala (Metals) dans VS Code

1. Ouvrir **Visual Studio Code**
2. Aller dans l’onglet **Extensions** (`Ctrl+Shift+X`)
3. Rechercher **Scala (Metals)** et l’installer

---

#### 3. 📂 Ouvrir le dossier `backend-pstl` dans VS Code

- Une fois ouvert, **Metals** proposera automatiquement d’importer le projet SBT  
- Accepter la proposition et attendre que l’import se termine

## 🚀 Lancement du projet
Toutes les commandes suivantes doivent être exécutées **depuis la racine du projet**, c’est-à-dire dans le dossier `Js-schema-generator-user-guided`.

### Étape 1 – Lancer le frontend

```bash
npm run frontend
```

### Étape 2 – Lancer le backend

```bash
npm run backend
```

