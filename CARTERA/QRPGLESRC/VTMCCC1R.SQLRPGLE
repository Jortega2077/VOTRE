***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC1R                                          *                         
     ** Descripciòn:  : Consulta de Compradoras por Nombre Menú Opi       *                         
     ** Proyecto      : Menú de Opciones Opi Paises                       *                         
     ** Autor         : (Jortega)                                         *                         
     ** Fecha Creaciòn: 23 de Diciembre de 2008                           *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FLmllst01  If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtccpi0p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DSqlStr           s           1000a   Inz(*Blanks)                                             
     DSqlNr1           s            500a   Inz(*Blanks)                                             
     DSqlNr2           s            500a   Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DSituar           s              4s 0 Inz(*Zeros)                                              
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DMsg1                             N   Overlay(Indic:38)                                        
     DMsg2                             N   Overlay(Indic:39)                                        
     DMsg3                             N   Overlay(Indic:37)                                        
     DMsg4                             N   Overlay(Indic:51)                                        
     DMsg5                             N   Overlay(Indic:52)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:40)                                        
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     DQmlzipc                              Like(Mlzipc)                                             
     DQmlcstÑ                              Like(MlcstÑ)                                             
     DQmlname                              Like(Mlname)                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DLlenarSfl        Pr                                                                           
     DBorraSfl         Pr                                                                           
     DPintarSfl        Pr                                                                           
     DSituarEn         Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPcedula                         9s 0                                                          
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPcedula                         9s 0                                                          
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
         PgmPais(Pcia:Xpais);                                                                       
         Dow Salir = *Off;                                                                          
          Pfecha = %Char(%Date());                                                                  
          PintarSfl();                                                                              
          If Nrr <= *Zeros And Salir = *Off And Pnom <> *Blanks;                                    
           Msg1 = *On;                                                                              
           Exfmt Rdato1;                                                                            
           Msg1 = *Off;                                                                             
           Clear Rcontrol;                                                                          
           Iter;                                                                                    
          ElseIf Nrr <= *Zeros And Salir = *Off And Pnom = *Blanks;                                 
           Msg2 = *On;                                                                              
          Endif;                                                                                    
          If Salir = *On;                                                                           
           Leave;                                                                                   
          Endif;                                                                                    
          If Cancelar = *On;                                                                        
           Clear Rcontrol;                                                                          
           Iter;                                                                                    
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
       // Arma la Estructura Requerida del Sql                                                      
       SqlNr1 = 'Select Mlzipc, MlcstÑ, Mlnam+                                                      
                e From Opf' + Pcia + '/Lmllst01';                                                   
       SqlNr2 = 'Where Mlname Like Trim(' + '''' + '%' + %Trim(Pnom) +                              
                '%' + '''' + ')' + ' Order By Mlzipc, Mlname ' +                                    
                'Fetch First 9999 Rows Only';                                                       
       //Arma el Sql a Consultar Dinamicamente                                                      
       SqlStr = %Trim(SqlNr1) + ' ' +  %Trim(SqlNr2);                                               
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur From :SqlStr;                                                                   
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor Cursor For Cur;                                                              
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor Into :Fields;                                                                  
                                                                                                    
        Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                     
         Write Rdato1;                                                                              
         Pop = *Blanks;                                                                             
         Pzon = %Trim(Qmlzipc);                                                                     
         Pced = QmlcstÑ;                                                                            
         Pnomb = Qmlname;                                                                           
         Nrr += 1;                                                                                  
         Posi = Nrr;                                                                                
         Situar = Nrr;                                                                              
         Write Rdatos;                                                                              
       Exec Sql                                                                                     
        Fetch Cursor Into :Fields;                                                                  
        Enddo;                                                                                      
        Msg3 = *Off;                                                                                
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor;                                                                               
                                                                                                    
        SflEnd = *On;                                                                               
        If Nrr > *Zeros;                                                                            
         Msg4 = *On;                                                                                
         SflDsp = *On;                                                                              
        Else;                                                                                       
         Msg4 = *Off;                                                                               
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
       Ppais = Xpais;                                                                               
       SflDspCtl = *On;                                                                             
       Write Rteclas;                                                                               
       Exfmt Rcontrol;                                                                              
       SflDspCtl = *Off;                                                                            
       Msg2 = *Off;                                                                                 
       If Salir = *Off And Cancelar = *Off And Nrr > *Zeros;                                        
        Readc Rdatos;                                                                               
         Dow Not %Eof();                                                                            
          Select;                                                                                   
           When Pop = '1';                                                                          
            Pcedula = Pced;                                                                         
            Salir = *On;                                                                            
            Leave;                                                                                  
          Endsl;                                                                                    
          Pop = *Blanks;                                                                            
          Update Rdatos;                                                                            
         Readc Rdatos;                                                                              
        Enddo;                                                                                      
        Endif;                                                                                      
       If Nrr > *Zeros And Salir = *Off And Cancelar = *Off And                                     
       Pzona <> *Blanks;                                                                            
        SituarEn();                                                                                 
        Pzona = *Blanks;                                                                            
       Endif;                                                                                       
       If Pnom <> *Blanks And Salir = *Off And Cancelar = *Off;                                     
        Msg3 = *On;                                                                                 
        LlenarSfl();                                                                                
        Pnom = *Blanks;                                                                             
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarSfl        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Situar En por Zona                                 //                       
       //------------------------------------------------------------------//                       
     PSituarEn         B                                                                            
     DSituarEn         Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DConta            s              4s 0 Inz(1)                                                   
     DOsw              s              1a   Inz(*Blanks)                                             
      /Free                                                                                         
       Dow Conta <= Situar;                                                                         
        Chain Conta Rdatos;                                                                         
        If Pzon = Pzona;                                                                            
         Posic = Conta;                                                                             
         Osw = 'S';                                                                                 
         Leave;                                                                                     
        Else;                                                                                       
         Conta += 1;                                                                                
        Endif;                                                                                      
       Enddo;                                                                                       
       If Osw = *Blanks;                                                                            
        Msg5 = *On;                                                                                 
        Exfmt Rdato1;                                                                               
        Msg5 = *Off;                                                                                
       Endif;                                                                                       
      /End-Free                                                                                     
     PSituarEn         E                                                                            
