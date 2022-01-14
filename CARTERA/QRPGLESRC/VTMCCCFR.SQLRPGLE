      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCFR                                          **                        
      * Descripción   : Pgm. Ingreso de Parámetro Compradoras Activas     **                        
      * Proyecto      : Menu Opciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
      * Fecha Creación: 06 de Abril de 2010                               **                        
      *                                                                   **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definición de Archivo de Pantallas                                                          
     FVtcpci7p  Cf   e             Workstn                                                          
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
     DPacc                            1a                                                            
     DPInd                            1a                                                            
     DPZon                         8000a                                                            
     DPuser                          10a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPacc                            1a                                                            
     DPInd                            1a                                                            
     DPZon                         8000a                                                            
     DPuser                          10a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       Puser = User;                                                                                
       BorrarFileQtemp();                                                                           
       CrearFileQtemp();                                                                            
       Popc = *Zeros;                                                                               
       Pind = *Blanks;                                                                              
       Dow Popc <> 3 And PInd <> 'S';                                                               
           *In30 = *On;                                                                             
           Clear Rdatos3;                                                                           
           Clear Rdatos7;                                                                           
           Clear Rdatos4;                                                                           
           SqlNr2 = *Blanks;                                                                        
           Write Rblanco;                                                                           
           If Pacc = 'A';                                                                           
            Exfmt Rdatos3;                                                                          
            *In55 = *On;                                                                            
           ElseIf Pacc = 'I';                                                                       
            Exfmt Rdatos7;                                                                          
            *In56 = *On;                                                                            
           Endif;                                                                                   
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
            Pind = 'S';                                                                             
            PZon = *Blanks;                                                                         
            SqlNr2 = *Blanks;                                                                       
            If Pacc = 'A';                                                                          
             SqlNr2 = '(Mldele = '' ''' + ' And Mldm01 = '' ''' +                                   
              ' ) and ( Mlzipc Not In (' + '''' + 'WEB' + '''' + ', '  +                            
                                           '''' + 'LIN' + '''' + ', '  +                            
                                           '''' + 'AMA' + '''' + ', '  +                            
                                           '''' + 'DAF' + '''' + ', '  +                            
                                           '''' + 'DLM' + '''' + ', '  +                            
                                           '''' + 'COR' + '''' + ', '  +                            
                                           '''' + 'RCD' + '''' + ', '  +                            
                                           '''' + 'RIP' + '''' + ', '  +                            
                                           '''' + 'MLI' + '''' + ')) '  ;                           
            ElseIf Pacc = 'I';                                                                      
             SqlNr2 = '(Mldele <> '' ''' + ' Or Mldm01 <> '' ''' +                                  
              ' ) and ( Mlzipc Not In (' + '''' + 'WEB' + '''' + ', '  +                            
                                           '''' + 'AMA' + '''' + ', '  +                            
                                           '''' + 'DAF' + '''' + ', '  +                            
                                           '''' + 'LIN' + '''' + ', '  +                            
                                           '''' + 'DLM' + '''' + ', '  +                            
                                           '''' + 'COR' + '''' + ', '  +                            
                                           '''' + 'RCD' + '''' + ', '  +                            
                                           '''' + 'RIP' + '''' + ', '  +                            
                                           '''' + 'MLI' + '''' + ')) '  ;                           
            Endif;                                                                                  
            PZon = %Trim(SqlNr2) + '|';                                                             
            Leave;                                                                                  
           Endif;                                                                                   
       Enddo;                                                                                       
       If Popc = 1;                                                                                 
        PZon = %Trim(Pzon) + '|';                                                                   
       Endif;                                                                                       
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
     ** Definición de Variables de Trabajo                                                          
     DOsw              s              1a   Inz('0')                                                 
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
       *In55 = *Off;                                                                                
       *In56 = *Off;                                                                                
       SflDspCtl = *Off;                                                                            
       If Salir = *Off And Cancelar = *Off And PInd <> 'S';                                         
          Osw = '0';                                                                                
          Readc Rdatos1;                                                                            
          Dow Not %Eof();                                                                           
           If Osw = '0';                                                                            
            SqlNr2 = *Blanks;                                                                       
            Osw = '1';                                                                              
            If Pacc = 'A';                                                                          
             SqlNr2 = '(Mldele = '' ''' + ' And Mldm01 = '' ''' + ') And (';                        
            ElseIf Pacc = 'I';                                                                      
             SqlNr2 = '(Mldele <> '' ''' + ' Or Mldm01 <> '' ''' + ') And (';                       
            Endif;                                                                                  
           Endif;                                                                                   
            Select;                                                                                 
             When Pzona <> *Blanks;                                                                 
              SqlNr2 = %Trim(SqlNr2) + ' Mlzipc = ' + '''' + Pzona + '''';                          
              Pzona = *Blanks;                                                                      
            Endsl;                                                                                  
            Readc Rdatos1;                                                                          
            If Pzona <> *Blanks;                                                                    
               SqlNr2 = %Trim(SqlNr2) + ' Or ';                                                     
            Endif;                                                                                  
          Enddo;                                                                                    
             If SqlNr2 <> *Blanks;                                                                  
                SqlNr2 = %Trim(SqlNr2) + ')';                                                       
                Pind = 'S';                                                                         
                PZon = %Trim(SqlNr2);                                                               
             Else;                                                                                  
                *In34 = *On;                                                                        
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
       If Pacc = 'A';                                                                               
        Strsql = 'Drop Table ' + %Trim(User) + '/vtwi2bf';                                          
       Endif;                                                                                       
       If Pacc = 'I';                                                                               
        Strsql = 'Drop Table ' + %Trim(User) + '/vtwi4bf';                                          
       Endif;                                                                                       
                                                                                                    
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
      /Free                                                                                         
       If Pacc = 'A';                                                                               
         Strsql = *Blanks;                                                                          
         Strsql = 'Create Table ' + %Trim(User) + '/vtwi2bf (' +                                    
         'WI2ZON CHAR(10), WI2CED NUMERIC(9), WI2CLL CHAR(30), +                                    
          WI2POS CHAR(30), +                                                                        
          WI2NAM CHAR(30), WI2CLS CHAR(3), WI2TL1 NUMERIC(15), +                                    
          WI2TL2 NUMERIC(15), WI2CEL NUMERIC(11), WI2MED CHAR(10), +                                
          WI2ME1 CHAR(10), WI2CRL NUMERIC(9), WI2DM1 CHAR(1), +                                     
          WI2DM3 CHAR(1), WI2DEL CHAR(1), WI2EMA CHAR(60), WI2DIR CHAR(180), +                      
          WI2DTR CHAR(2), WI2CDR CHAR(20), WI2PRR CHAR(60), +                                       
          WI2BRR CHAR(30), WI2CPR CHAR(10),+                                                        
          WI2CDD CHAR(20), WI2DID CHAR(180), WI2DTD CHAR(2), +                                      
          WI2PRD CHAR(60), +                                                                        
          WI2BRD CHAR(30), WI2CPD CHAR(10), +                                                       
          WI2PRL CHAR(3), +                                                                         
          WI2DPB CHAR(120), +                                                                       
          WI2CPB CHAR(60), +                                                                        
          WI2ZPB CHAR(60), ' +                                                                      
          'WI2D01 CHAR(1), WI2DM2 CHAR(1), WI2D03 CHAR(1), ' +                                      
          'WI2DM4 CHAR(1), WI2DM5 CHAR(1), WI2DM6 CHAR(1), ' +                                      
          'WI2DM7 CHAR(1), WI2DM8 CHAR(1), WI2DM9 CHAR(1), ' +                                      
          'WI2D10 CHAR(1), WI2D11 CHAR(1), WI2D12 CHAR(1), ' +                                      
          'WI2D13 CHAR(1), WI2D14 CHAR(1), WI2D15 CHAR(1), ' +                                      
          'WI2D16 CHAR(1), WI2D17 CHAR(1), WI2D18 CHAR(1), ' +                                      
          'WI2D19 CHAR(1), WI2D20 CHAR(1), WI2DTP CHAR(40), ' +                                     
          'WI2MLP CHAR(3), ' +                                                                      
          'Constraint ' + %Trim(User) + '/vtwi2bf_WI2CED_00001 ' +                                  
           'Primary Key(WI2CED))';                                                                  
       Endif;                                                                                       
                                                                                                    
       If Pacc = 'I';                                                                               
         Strsql = *Blanks;                                                                          
         Strsql = 'Create Table ' + %Trim(User) + '/vtwi4bf (' +                                    
         'WI4ZON CHAR(10), WI4CED NUMERIC(9), WI4CLL CHAR(30), +                                    
          WI4POS CHAR(30), +                                                                        
          WI4NAM CHAR(30), WI4CLS CHAR(3), WI4TL1 NUMERIC(15), +                                    
          WI4TL2 NUMERIC(15), WI4CEL NUMERIC(11), WI4MED CHAR(10), +                                
          WI4ME1 CHAR(10), WI4CRL NUMERIC(9), WI4DM1 CHAR(1), +                                     
          WI4DM3 CHAR(1), WI4DEL CHAR(1), WI4EMA CHAR(60), WI4DIR CHAR(180), +                      
          WI4DTR CHAR(2), WI4CDR CHAR(20),  WI4PRR CHAR(60), +                                      
          WI4BRR CHAR(30), WI4CPR CHAR(10), +                                                       
          WI4CDD CHAR(20), WI4DID CHAR(180), WI4DTD CHAR(2), +                                      
          WI4PRD CHAR(60), +                                                                        
          WI4BRD CHAR(30), WI4CPD CHAR(10), WI4PRL CHAR(3), '+                                      
          'WI4D01 CHAR(1), WI4DM2 CHAR(1), WI4D03 CHAR(1), ' +                                      
          'WI4DM4 CHAR(1), WI4DM5 CHAR(1), WI4DM6 CHAR(1), ' +                                      
          'WI4DM7 CHAR(1), WI4DM8 CHAR(1), WI4DM9 CHAR(1), ' +                                      
          'WI4D10 CHAR(1), WI4D11 CHAR(1), WI4D12 CHAR(1), ' +                                      
          'WI4D13 CHAR(1), WI4D14 CHAR(1), WI4D15 CHAR(1), ' +                                      
          'WI4D16 CHAR(1), WI4D17 CHAR(1), WI4D18 CHAR(1), ' +                                      
          'WI4D19 CHAR(1), WI4D20 CHAR(1), WI4DTP CHAR(40), ' +                                     
          'WI4MLP CHAR(1), ' +                                                                      
          'Constraint ' + %Trim(User) + '/vtwi4bf_WI4CED_00001 ' +                                  
           'Primary Key(WI4CED))';                                                                  
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :Strsql;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Execute Cur1;                                                                               
                                                                                                    
      /End-Free                                                                                     
     PCrearFileQtemp   E                                                                            
