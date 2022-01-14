      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCNR                                          **                        
      * Descripción   : Pgm. Ingreso de Parámetro Inscripciones Diarias.  **                        
      * Proyecto      : Menu Opciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
      * Fecha Creación: 06 de Abril de 2010                               **                        
      *                                                                   **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Archivo de Pantallas                                                          
     FVtcpci0p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos1:Nrr)                                       
                                                                                                    
     ** Definición de Variables de Trabajo                                                          
     DSqlNr2           s           8000a   Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DUser             s             10a   Inz(*User)                                               
     DStrsql           s          25000a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DSflInz                           N   Overlay(Indic:70)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DBorrarFileQtemp  Pr                                                                           
     DCrearFileQtemp   Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPInd                            1a                                                            
     DPcam                            4a                                                            
     DPZon                         8000a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPInd                            1a                                                            
     DPcam                            4a                                                            
     DPZon                         8000a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       BorrarFileQtemp();                                                                           
       CrearFileQtemp();                                                                            
       Dow Popc <> 3 And PInd <> 'S';                                                               
           *In30 = *On;                                                                             
           Clear Rdatos3;                                                                           
           Clear Rdatos4;                                                                           
           Pcampa = *Blanks;                                                                        
           SqlNr2 = *Blanks;                                                                        
           Write Rblanco;                                                                           
           Exfmt Rdatos3;                                                                           
           PgmPais(Pcia:Xpais);                                                                     
           If Popc = 1;                                                                             
              BorraSfl();                                                                           
              Dow Salir = *Off And PInd <> 'S';                                                     
                  Pfecha = %Char(%Date());                                                          
                  PintarSfl();                                                                      
                  If Salir = *On;                                                                   
                     Pind = 'N';                                                                    
                     Leave;                                                                         
                  Endif;                                                                            
                  If Cancelar = *On;                                                                
                     Clear Rcontrol1;                                                               
                     BorraSfl();                                                                    
                     *In30 = *On;                                                                   
                     Iter;                                                                          
                  Endif;                                                                            
              Enddo;                                                                                
              If Salir = *On;                                                                       
                 Salir = *Off;                                                                      
              Endif;                                                                                
           Endif;                                                                                   
           If Popc = 2;                                                                             
              Clear Rdatos4;                                                                        
              Dow Salir = *Off;                                                                     
                  Exfmt Rdatos4;                                                                    
                  *In33 = *Off;                                                                     
                  If Salir = *On;                                                                   
                     PInd = 'N';                                                                    
                     Leave;                                                                         
                  Endif;                                                                            
                  If Cancelar = *On;                                                                
                     Clear Rdatos4;                                                                 
                     Iter;                                                                          
                  Endif;                                                                            
                  If Pcampa <> *Blanks And Salir = *Off And Cancelar = *Off;                        
                     PInd = 'S';                                                                    
                     Pcam = Pcampa;                                                                 
                     Clear Rdato2;                                                                  
                     Leave;                                                                         
                  Else;                                                                             
                     PInd = 'N';                                                                    
                     *In33 = *On;                                                                   
                  Endif;                                                                            
              Enddo;                                                                                
              If Salir = *On;                                                                       
                 Salir = *Off;                                                                      
              Endif;                                                                                
           Endif;                                                                                   
       Enddo;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo del Sub File              //                       
       //------------------------------------------------------------------//                       
     PLlenarSfl        B                                                                            
     DLlenarSfl        Pi                                                                           
      /Free                                                                                         
        SflDsp = *On;                                                                               
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
       SflInz = *On;                                                                                
       SflClr = *On;                                                                                
       Write Rcontrol1;                                                                             
       SflInz = *Off;                                                                               
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
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rblanco;                                                                               
       Write Rteclas1;                                                                              
       Exfmt Rcontrol1;                                                                             
       *In30 = *Off;                                                                                
       *In33 = *Off;                                                                                
       *In34 = *Off;                                                                                
       SflDspCtl = *Off;                                                                            
       If Salir = *Off And Cancelar = *Off And PInd <> 'S';                                         
          Readc Rdatos1;                                                                            
          Dow Not %Eof();                                                                           
            Select;                                                                                 
             When Pzona <> *Blanks;                                                                 
              SqlNr2 = %Trim(SqlNr2) + ' A.Mlzipc = ' + '''' + Pzona + '''';                        
              Pzona = *Blanks;                                                                      
            Endsl;                                                                                  
            Readc Rdatos1;                                                                          
            If Pzona <> *Blanks;                                                                    
               SqlNr2 = %Trim(SqlNr2) + ' Or ';                                                     
            Endif;                                                                                  
          Enddo;                                                                                    
          If Pcampa <> *Blanks;                                                                     
             If SqlNr2 <> *Blanks;                                                                  
                SqlNr2 = '(' + %Trim(SqlNr2) + ')';                                                 
                Pind = 'S';                                                                         
                PZon = %Trim(SqlNr2);                                                               
                PCam = PCampa;                                                                      
                //Salir = *On;                                                                      
             Else;                                                                                  
                *In34 = *On;                                                                        
             Endif;                                                                                 
          Else;                                                                                     
             *In30 = *On;                                                                           
             *In33 = *On;                                                                           
          Endif;                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Archivo Creado en la Qtemp               //                       
       //------------------------------------------------------------------//                       
     PBorrarFileQtemp  B                                                                            
     DBorrarFileQtemp  Pi                                                                           
      /Free                                                                                         
        Strsql = *Blanks;                                                                           
        Strsql = 'Drop Table ' + %Trim(User) + '/Vtwisdf';                                          
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :Strsql;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Cur0;                                                                               
                                                                                                    
      /End-Free                                                                                     
     PBorrarFileQtemp  E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea Archivo en la Qtemp                       //                       
       //------------------------------------------------------------------//                       
     PCrearFileQtemp   B                                                                            
     DCrearFileQtemp   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCom              s              4a   Inz('''')                                                
      /Free                                                                                         
        Strsql = *Blanks;                                                                           
        Strsql = 'Create Table ' + %Trim(User) + '/Vtwisdf(' +                                      
                 'Isddel Char(1) Ccsid 284 Default Null, ' +                                        
                 'Isdcia Char(3) Ccsid 284 Default Null, ' +                                        
                 'Isdzon Char(3) Ccsid 284 Default Null, ' +                                        
                 'Isdced Numeric(9, 0) Default Null, ' +                                            
                 'Isdcdl Char(30) Ccsid 284 Default Null, ' +                                       
                 'Isdpos Char(30) CcSID 284 Default Null, ' +                                       
                 'Isdnom Char(30) CcSID 284 Default Null, ' +                                       
                 'Isdcus Char(3) CcSID 284 Default Null, ' +                                        
                 'Isddmg Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddrr For Column Isdddr Char(180) Ccsid 284 Default Null, ' +                    
                 'Isddrd Char(180) Ccsid 284 Default Null, ' +                                      
                 'Isdciu Char(20) Ccsid 284 Default Null, ' +                                       
                 'Isdcrt Numeric(9, 0) Default Null, ' +                                            
                 'Isdcor Char(4) Ccsid 284 Default Null, ' +                                        
                 'Isdcbj Char(4) Ccsid 284 Default Null, ' +                                        
                 'Isdest Char(2) Ccsid 284 Default Null, ' +                                        
                 'Isddm1 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm2 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm3 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm4 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm5 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm6 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm7 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm8 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isddm9 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd10 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd11 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd12 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd13 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd14 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd15 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd16 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd17 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd18 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd19 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdd20 Char(1) CcSID 284 Default Null, ' +                                        
                 'Isdmpl Char(3) CcSID 284 Default Null) ' +                                        
                 'Rcdfmt Regisd';                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :Strsql;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Cur1;                                                                               
                                                                                                    
       Strsql = *Blanks;                                                                            
       Strsql = 'Label On Table ' + %Trim(User) + '/Vtwisdf Is ' +                                  
                Com + 'Archivo de Trabajo Inscripciones Diarias' + Com;                             
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur2 From :Strsql;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Cur2;                                                                               
                                                                                                    
       Strsql = *Blanks;                                                                            
       Strsql = 'Label On Column ' + %Trim(User) + '/Vtwisdf( ' +                                   
                'Isddel Is ' + Com + 'Deleteada            ' + Com + ', ' +                         
                'Isdcia Is ' + Com + 'Compañia             ' + Com + ', ' +                         
                'Isdzon Is ' + Com + 'Zona                 ' + Com + ', ' +                         
                'Isdced Is ' + Com + 'Cedula               ' + Com + ', ' +                         
                'Isdcdl Is ' + Com + 'Identificación      ' + Com + ', ' +                          
                'Isdpos Is ' + Com + 'Position             ' + Com + ', ' +                         
                'Isdnom Is ' + Com + 'Nombre               ' + Com + ', ' +                         
                'Isdcus Is ' + Com + 'Customer Class       ' + Com + ', ' +                         
                'Isddmg Is ' + Com + 'Tipo Inscripción     ' + Com + ', ' +                         
                'Isddrr Is ' + Com + 'Dirección Residencia ' + Com + ', ' +                         
                'Isddrd Is ' + Com + 'Dirección Despacho   ' + Com + ', ' +                         
                'Isdciu Is ' + Com + 'Ciudad               ' + Com + ', ' +                         
                'Isdcrt Is ' + Com + 'Credito Limite       ' + Com + ', ' +                         
                'Isdcor Is ' + Com + 'Campaña Origen       ' + Com + ', ' +                         
                'Isdcbj Is ' + Com + 'Campaña de Baja      ' + Com + ', ' +                         
                'Isdest Is ' + Com + 'CO                   ' + Com + ', ' +                         
                'Isddm1 Is ' + Com + 'Demo1                ' + Com + ', ' +                         
                'Isddm2 Is ' + Com + 'Demo2                ' + Com + ', ' +                         
                'Isddm3 Is ' + Com + 'Demo3                ' + Com + ', ' +                         
                'Isddm4 Is ' + Com + 'Demo4                ' + Com + ', ' +                         
                'Isddm5 Is ' + Com + 'Demo5                ' + Com + ', ' +                         
                'Isddm6 Is ' + Com + 'Demo6                ' + Com + ', ' +                         
                'Isddm7 Is ' + Com + 'Demo7                ' + Com + ', ' +                         
                'Isddm8 Is ' + Com + 'Demo8                ' + Com + ', ' +                         
                'Isddm9 Is ' + Com + 'Demo9                ' + Com + ', ' +                         
                'Isdd10 Is ' + Com + 'Demo10               ' + Com + ', ' +                         
                'Isdd11 Is ' + Com + 'Demo11               ' + Com + ', ' +                         
                'Isdd12 Is ' + Com + 'Demo12               ' + Com + ', ' +                         
                'Isdd13 Is ' + Com + 'Demo13               ' + Com + ', ' +                         
                'Isdd14 Is ' + Com + 'Demo14               ' + Com + ', ' +                         
                'Isdd15 Is ' + Com + 'Demo15               ' + Com + ', ' +                         
                'Isdd16 Is ' + Com + 'Demo16               ' + Com + ', ' +                         
                'Isdd17 Is ' + Com + 'Demo17               ' + Com + ', ' +                         
                'Isdd18 Is ' + Com + 'Demo18               ' + Com + ', ' +                         
                'Isdd19 Is ' + Com + 'Demo19               ' + Com + ', ' +                         
                'Isdd20 Is ' + Com + 'Demo20               ' + Com + ', ' +                         
                'Isdmpl Is ' + Com + 'Mail Plan            ' + Com + ')';                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3 From :Strsql;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Cur3;                                                                               
                                                                                                    
       Strsql = *Blanks;                                                                            
       Strsql = 'Label On Column ' + %Trim(User) + '/Vtwisdf(' +                                    
          'Isddel Text Is ' + Com + 'Deleteada            ' + Com + ', ' +                          
          'Isdcia Text Is ' + Com + 'Compañia             ' + Com + ', ' +                          
          'Isdzon Text Is ' + Com + 'Zona                 ' + Com + ', ' +                          
          'Isdced Text Is ' + Com + 'Cedula               ' + Com + ', ' +                          
          'Isdcdl Text Is ' + Com + 'Identificación      ' + Com + ', ' +                           
          'Isdpos Text Is ' + Com + 'Position             ' + Com + ', ' +                          
          'Isdnom Text Is ' + Com + 'Nombre               ' + Com + ', ' +                          
          'Isdcus Text Is ' + Com + 'Customer Class       ' + Com + ', ' +                          
          'Isddmg Text Is ' + Com + 'Tipo Inscripción     ' + Com + ', ' +                          
          'Isddrr Text Is ' + Com + 'Dirección Residencia ' + Com + ', ' +                          
          'Isddrd Text Is ' + Com + 'Dirección Despacho   ' + Com + ', ' +                          
          'Isdciu Text Is ' + Com + 'Ciudad               ' + Com + ', ' +                          
          'Isdcrt Text Is ' + Com + 'Credito Limite       ' + Com + ', ' +                          
          'Isdcor Text Is ' + Com + 'Campaña Origen       ' + Com + ', ' +                          
          'Isdcbj Text Is ' + Com + 'Campaña de Baja      ' + Com + ',  '+                          
          'Isdest Text Is ' + Com + 'CO                   ' + Com + ', '+                           
          'Isddm1 Text Is ' + Com + 'Demo1                ' + Com + ', '+                           
          'Isddm2 Text Is ' + Com + 'Demo2                ' + Com + ', '+                           
          'Isddm3 Text Is ' + Com + 'Demo3                ' + Com + ', '+                           
          'Isddm4 Text Is ' + Com + 'Demo4                ' + Com + ', '+                           
          'Isddm5 Text Is ' + Com + 'Demo5                ' + Com + ', '+                           
          'Isddm6 Text Is ' + Com + 'Demo6                ' + Com + ', '+                           
          'Isddm7 Text Is ' + Com + 'Demo7                ' + Com + ', '+                           
          'Isddm8 Text Is ' + Com + 'Demo8                ' + Com + ', '+                           
          'Isddm9 Text Is ' + Com + 'Demo9                ' + Com + ', '+                           
          'Isdd10 Text Is ' + Com + 'Demo10               ' + Com + ', '+                           
          'Isdd11 Text Is ' + Com + 'Demo11               ' + Com + ', '+                           
          'Isdd12 Text Is ' + Com + 'Demo12               ' + Com + ', '+                           
          'Isdd13 Text Is ' + Com + 'Demo13               ' + Com + ', '+                           
          'Isdd14 Text Is ' + Com + 'Demo14               ' + Com + ', '+                           
          'Isdd15 Text Is ' + Com + 'Demo15               ' + Com + ', '+                           
          'Isdd16 Text Is ' + Com + 'Demo16               ' + Com + ', '+                           
          'Isdd17 Text Is ' + Com + 'Demo17               ' + Com + ', '+                           
          'Isdd18 Text Is ' + Com + 'Demo18               ' + Com + ', '+                           
          'Isdd19 Text Is ' + Com + 'Demo19               ' + Com + ', '+                           
          'Isdd20 Text Is ' + Com + 'Demo20               ' + Com + ', '+                           
          'Isdmpl Text Is ' + Com + 'Mail Plan            ' + Com + ')';                            
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur4 From :Strsql;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Cur4;                                                                               
                                                                                                    
      /End-Free                                                                                     
     PCrearFileQtemp   E                                                                            
