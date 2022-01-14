***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCOR                                          *                         
     ** Descripciòn:  : Consulta Cantidades Negativas por Sku             *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 21 de Septiembre de 2010                          *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcrni0p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DStrSql           s           1000a   Inz(*Blanks)                                             
     DWNrr             S              5s 0 Inz(0)                                                   
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DIqitem                         15a                                                            
     DIqwhse                          3a                                                            
     DIqqtya                          5  0                                                          
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DPgmNegativos     Pr                  Extpgm('VOTREP00/VTACRC3L')                              
     DXcia                            3                                                             
     DXsku                           15                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         PgmPais(Pcia:Xpais);                                                                       
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
          // Procesa todos                                                                          
          If *In07 = *On;                                                                           
            Exfmt RDATO2;                                                                           
            If P2OPC = 'S';                                                                         
              ProcTodos();                                                                          
            Endif;                                                                                  
            *In07 = *Off;                                                                           
            Iter;                                                                                   
          Endif;                                                                                    
         If Salir = *Off And Nrr > *Zeros;                                                          
          Readc Rdatos;                                                                             
          Dow Not %Eof();                                                                           
           Select;                                                                                  
            When Pop = '1';                                                                         
             PgmNegativos(Pcia:Psku);                                                               
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos;                                                                            
          Readc Rdatos;                                                                             
         Enddo;                                                                                     
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
       BorraSfl();                                                                                  
       StrSql = *Blanks;                                                                            
       StrSql = 'Select Iqitem, Iqwhse, Iqqtya ' +                                                  
                'From Opf' + Pcia + '/Linqty01 Inner Join ' +                                       
                'Opf' + Pcia + '/Linvnt01 On Ivitem = Iqitem ' +                                    
                'Where (Iqqtym < 0 And Ivstck = ''Y''' + ' And ' +                                  
                'Ivkitp = ''N''' + ') Or (Iqqtya < 0 And Ivstck = ''Y''' +                          
                ' And ' + 'Ivkitp = ''N''' + ')';                                                   
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur0 From :StrSql;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Declare Cursor0 Cursor For Cur0;                                                           
                                                                                                    
        Exec Sql                                                                                    
         Open Cursor0;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch Cursor0 Into :Fields;                                                                
                                                                                                    
        Dow Sqlcod <> 100 And SqlCod >= *Zeros;                                                     
         Pop = *Blanks;                                                                             
         Psku = Iqitem;                                                                             
         Pbod = Iqwhse;                                                                             
         Puni = Iqqtya;                                                                             
         Nrr += 1;                                                                                  
         Posi = Nrr;                                                                                
         Write Rdatos;                                                                              
         Exec Sql                                                                                   
          Fetch Cursor0 Into :Fields;                                                               
        Enddo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close Cursor0;                                                                             
                                                                                                    
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
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rteclas;                                                                               
       Exfmt Rcontrol;                                                                              
       SflDspCtl = *Off;                                                                            
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
       //------------------------------------------------------------------//                       
       // Procesa todos los registros                                       /                       
       //------------------------------------------------------------------//                       
     PProcTodos        B                                                                            
     DProcTodos        Pi                                                                           
      /Free                                                                                         
         WNrr = 1;                                                                                  
         Clear Rdatos;                                                                              
         CHain WNrr Rdatos;                                                                         
         Dow Psku <> ' ';                                                                           
           PgmNegativos(Pcia:Psku);                                                                 
           WNrr = WNrr + 1;                                                                         
           Clear Rdatos;                                                                            
         CHain WNrr Rdatos;                                                                         
         Enddo;                                                                                     
      /End-Free                                                                                     
     PProcTodos        E                                                                            
