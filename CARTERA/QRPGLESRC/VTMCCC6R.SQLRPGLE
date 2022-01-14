***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC6R                                          *                         
     ** Descripciòn:  : Consulta de Archivos a Subir (Bancos)             *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 13 de Enero de 2009                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No) Actgrp(*New)                                                
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtbnci2p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DPosix            s              4s 0 Inz(*Zeros)                                              
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DDir              s               *                                                            
     DMsg              s             52a                                                            
     DP_Dirent         s               *                                                            
     DFile             s             20a   Inz(*Blanks)                                             
     DCmd              s           3000a                                                            
     DLen              s             15p 5                                                          
     DPos              s              3s 0 Inz(*Zeros)                                              
     DRuta             s             70a   Inz(*Blanks)                                             
     DRutaIfs          s            100                                                             
     DFechaAc          s              8                                                             
     DNombreRIfsT      s             30    Inz(*Blanks)                                             
     DFechaIfsT        s             10    Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            n   Overlay(Indic:3)                                         
     DCancelar                         n   Overlay(Indic:12)                                        
     DSflDsp                           n   Overlay(Indic:85)                                        
     DSflDspCtl                        n   Overlay(Indic:80)                                        
     DSflClr                           n   Overlay(Indic:75)                                        
     DSflEnd                           n   Overlay(Indic:40)                                        
                                                                                                    
     DDirent           Ds                  Based(P_dirent)                                          
     Dd_reserved1                    16a                                                            
     D   D_fileno_gen_id...                                                                         
     D   D_fil                       10u 0                                                          
     D   D_fileno                    10u 0                                                          
     D   D_reclen                    10u 0                                                          
     D   D_reserved3                 10i 0                                                          
     D   D_reserved4                  8a                                                            
     D   D_nlsinfo                   12a                                                            
     D     Nls_ccsid                 10i 0 Overlay(D_nlsinfo:1)                                     
     D     Nls_cntry                  2a   Overlay(D_nlsinfo:5)                                     
     D     Nls_lang                   3a   Overlay(D_nlsinfo:7)                                     
     D     Nls_reserv                 3a   Overlay(D_nlsinfo:10)                                    
     D   D_namelen                   10u 0                                                          
     D   D_name                     640a                                                            
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DGenerar          Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DVisualizar       Pr                                                                           
     DCrearTabla       Pr                                                                           
     DBorrarTabla      Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DOpendir          Pr              *   Extproc('opendir')                                       
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
     DReaddir          Pr              *   Extproc('readdir')                                       
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
     DClosedir         Pr            10i 0 Extproc('closedir')                                      
     DDirname                          *   Value Options(*String)                                   
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
     DAtrIfs           Pr                  ExtPgm('VOTREP00/VTMGOBJR')                              
     DRutaIfs                       100                                                             
     DFechaAc                         8                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                          20a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPrut                           64a                                                            
     DPfile                          20a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         PgmPais(Pcia:Xpais);                                                                       
         BorrarTabla();                                                                             
         CrearTabla();                                                                              
         Generar();                                                                                 
         Dow Salir = *Off;                                                                          
          Pfecha = %Char(%Date());                                                                  
          PintarSfl();                                                                              
          If Nrr <= *Zeros And Salir = *Off;                                                        
           *In38 = *On;                                                                             
           Exfmt Rdato1;                                                                            
           *In38 = *Off;                                                                            
           Iter;                                                                                    
          Endif;                                                                                    
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
          If Cancelar = *On;                                                                        
           Posix = *Zeros;                                                                          
           Clear Rcontrol;                                                                          
           Iter;                                                                                    
          Endif;                                                                                    
         Enddo;                                                                                     
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Genera la Información para el Sub File         //                       
       //------------------------------------------------------------------//                       
     PGenerar          B                                                                            
     DGenerar          Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DPos1             s              3s 0 Inz(*Zeros)                                              
     DTfecha           s             10a   Inz(*Blanks)                                             
      /Free                                                                                         
       Ruta = *Blanks;                                                                              
       Ruta = %Trim(Prut);                                                                          
       Pos = %Scan(' ':Ruta);                                                                       
       Dir = Opendir(%Subst(Ruta:1:Pos-1));                                                         
       If Dir = *Null;                                                                              
        *In39 = *On;                                                                                
        Exfmt Rdato1;                                                                               
        *In39 = *Off;                                                                               
        Salir = *On;                                                                                
       Endif;                                                                                       
       If Dir <> *Null;                                                                             
       P_dirent = Readdir(Dir);                                                                     
       Dow P_dirent <> *Null;                                                                       
        File = %Subst(D_name:1:D_namelen);                                                          
        Pos1 = %Scan('.':File);                                                                     
        If %Len(%Trim(File)) > *Zeros And Pos1 > 1;                                                 
         FechaAc = *Blanks;                                                                         
         RutaIfs = %Subst(Ruta:1:Pos-1) + '/' + %Trim(File);                                        
         AtrIfs(RutaIfs:FechaAc);                                                                   
         Tfecha = *Blanks;                                                                          
         Tfecha = %Subst(FechaAc:1:4) + '-' + %Subst(FechaAc:5:2) + '-' +                           
                   %Subst(FechaAc:7:2);                                                             
         Exec Sql                                                                                   
          Insert Into Qtemp/FilesIfs (NombreRIfs, FechaIfs)                                         
          Values(:File, :Tfecha);                                                                   
        Endif;                                                                                      
        P_dirent = Readdir(Dir);                                                                    
       Enddo;                                                                                       
       Endif;                                                                                       
       Closedir(Dir);                                                                               
      /End-Free                                                                                     
     PGenerar          E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSfl        B                                                                            
     DLlenarSfl        Pi                                                                           
      /Free                                                                                         
       BorraSfl();                                                                                  
       Exec Sql                                                                                     
       Declare C1 Scroll Cursor For                                                                 
       Select NombreRIfs As NombreRIfsT, FechaIfs As FechaIfsT                                      
       From Qtemp/FilesIfs                                                                          
       Order By FechaIfs Desc;                                                                      
                                                                                                    
       Exec Sql                                                                                     
        Open C1;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :NombreRIfsT, :FechaIfsT;                                                     
                                                                                                    
       Dow Sqlcod <> 100;                                                                           
        Pop = *Blanks;                                                                              
        Parch = NombreRIfsT;                                                                        
        Pfec = FechaIfsT;                                                                           
        Nrr += 1;                                                                                   
        Posi = Nrr;                                                                                 
        Write Rdatos;                                                                               
        Exec Sql                                                                                    
         Fetch C1 Into :NombreRIfsT, :FechaIfsT;                                                    
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
       If Nrr > *Zeros;                                                                             
        *In99 = *On;                                                                                
        Nrr += 1;                                                                                   
        Pfec = *Blanks;                                                                             
        Parch = 'Total Archivos: ' + %Trim(%Editc((Nrr-1):'K'));                                    
        Write Rdatos;                                                                               
        *In99 = *Off;                                                                               
       Endif;                                                                                       
       SflEnd = *On;                                                                                
        If Nrr > *Zeros;                                                                            
         SflDsp = *On;                                                                              
        Else;                                                                                       
         SflDsp = *Off;                                                                             
        Endif;                                                                                      
      /End-Free                                                                                     
     PLlenarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PBorraSfl         B                                                                            
     DBorraSfl         Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write Rcontrol;                                                                              
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorraSfl         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPintarSfl        B                                                                            
     DPintarSfl        Pi                                                                           
      /Free                                                                                         
       LlenarSfl();                                                                                 
       Pruta = %Trim(Prut);                                                                         
       If Salir = *Off;                                                                             
        If Posix > *Zeros;                                                                          
         Posic = Posix;                                                                             
        Endif;                                                                                      
        Ppais = Xpais;                                                                              
        SflDspCtl = *On;                                                                            
        Write Rteclas;                                                                              
        Exfmt Rcontrol;                                                                             
        SflDspCtl = *Off;                                                                           
        If Nrr > *Zeros And Salir = *Off And Cancelar = *Off;                                       
        Readc Rdatos;                                                                               
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            Pfile = Parch;                                                                          
            Salir = *On;                                                                            
           When Pop = '5';                                                                          
            Posix = Posi;                                                                           
            Visualizar();                                                                           
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos;                                                                            
         Readc Rdatos;                                                                              
        Enddo;                                                                                      
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Visualiza el Contenido de un Archivo           //                       
       //------------------------------------------------------------------//                       
     PVisualizar       B                                                                            
     DVisualizar       Pi                                                                           
      /Free                                                                                         
       Cmd = 'DSPF STMF(' +  ''''  + %Subst(Ruta:1:Pos-1) + '/' +                                   
              %Trim(Parch) + '''' + ')';                                                            
       Qcmdexc(Cmd:%Size(Cmd));                                                                     
      /End-Free                                                                                     
     PVisualizar       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qtemp             //                       
       //------------------------------------------------------------------//                       
     PCrearTabla       B                                                                            
     DCrearTabla       Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Create Table Qtemp/FilesIfs (                                                               
        NombreRIfs Char(30),                                                                        
        FechaIfs Char(10));                                                                         
      /End-Free                                                                                     
     PCrearTabla       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crear Tabla de Trabajo en la Qtemp             //                       
       //------------------------------------------------------------------//                       
     PBorrarTabla      B                                                                            
     DBorrarTabla      Pi                                                                           
      /Free                                                                                         
       Exec Sql                                                                                     
        Drop Table Qtemp/FilesIfs;                                                                  
      /End-Free                                                                                     
     PBorrarTabla      E                                                                            
