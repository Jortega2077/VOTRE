      **********************************************************************                        
      ** Programa     : VTPSGBKR                                          **                        
      ** Descripciòn: : Selección de campaña - Metas y Resultados de Guias**                        
      **                                                                  **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Edgar Ivan Maldonado Perez - PersonalSoft         **                        
      ** Fecha Creaciòn: 21 de Junio de 2016                              **                        
      **********************************************************************                        
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Deficion de Archivo de Pantalla                                                              
     FVTPSGIKP1 Cf   e             Workstn                                                          
     F                                     Sfile(RDatos1:Nrr)                                       
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('VTPSGBKR')                                       
     DWind                            1a                                                            
                                                                                                    
     DMain             Pi                                                                           
     DWind                            1a                                                            
                                                                                                    
      *Definición de variables de trabajo                                                           
     DPosix            s              4  0 Inz(*Zeros)                                              
     DI                s              4  0 Inz(*Zeros)                                              
     DPtr              s               *   Inz(%Addr(*In))                                          
     D$coma            s              4a   Inz('''')                                                
     DPcamr            s              4a   Inz(*blanks)                                             
     DPcame            s              4a   Inz(*blanks)                                             
     Dpoper            s              1a                                                            
     Dp_cia            s              3a                                                            
     Dpnume            s              3a                                                            
     DW_PaisControl    s              3                                                             
     DW_NombrePais     s             30                                                             
     DWNombre          s             30                                                             
     DWsql             s            500a   Inz(*Blanks)                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DOcultarCmp                       N   Overlay(Indic:33)                                        
     DPosicionarZ                      N   Overlay(Indic:34)                                        
     DPMetas                           N   Overlay(Indic:35)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:70)                                        
                                                                                                    
      *Definición de Vectores                                                                       
     D  VdesTpo        S             12A   DIM(5)CTDATA PERRCD(1)                                   
                                                                                                    
      *Definición de Estructuras                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPintarfile       Pr                                                                           
     DBorrarfile       Pr                                                                           
     DLeerOpcion       Pr                                                                           
                                                                                                    
      //programas externos                                                                          
     DPgmcampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DXcia                            3                                                             
     DPcamE                           4                                                             
     DPcamR                           4                                                             
     DPoper                           1                                                             
     DPnume                           3                                                             
                                                                                                    
      //trae la campaña operativa                                                                   
     DPgmcampo         Pr                  Extpgm('VOTREP00/VTMCSC0R')                              
     DXcia                            3                                                             
     DPcamO                           4                                                             
                                                                                                    
      //Invoca Programa Detalle de Metas                                                            
     DPgmMetas         Pr                  Extpgm('VOTREP00/VTPSGBLR')                              
     DXcia                            3                                                             
     DXcam                            4                                                             
     DXzon                            3                                                             
     DXnom                           30                                                             
                                                                                                    
      //Invoca Programa Resumido de Resultados                                                      
     DPgmResultados    Pr                  Extpgm('VOTREP00/VTPSGBGR')                              
     DXcia                            3                                                             
     DXcam                            4                                                             
                                                                                                    
      //programa pais                                                                               
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMGOI8R')                              
     DXcia                            3                                                             
     DXpais                          30A                                                            
                                                                                                    
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
                                                                                                    
       If Wind = 'M' Or Wind = 'R';  //Metas o Resultados                                           
          Dow Not Salir;                                                                            
            Pfecha = %Char(%Date());                                                                
            Ppgm = Nomp;                                                                            
            PNomp = W_NombrePais;                                                                   
            If Wind = 'M';                                                                          
               PMetas = *on;                                                                        
            EndIf;                                                                                  
            SflDspCtl = *On;                                                                        
            Write Teclas1;                                                                          
            Exfmt RControl1;                                                                        
            SflDspCtl = *Off;                                                                       
            If Salir;                                                                               
              Leave;                                                                                
            EndIf;                                                                                  
            If Nrr > *Zeros And Salir = *Off And Cancelar = *Off;                                   
              LeerOpcion();                                                                         
            Endif;                                                                                  
                                                                                                    
            If Cancelar;                                                                            
              Clear RControl1;                                                                      
              Clear W_NombrePais;                                                                   
              Clear W_PaisControl;                                                                  
            Else;                                                                                   
              If Ppais = 1;                                                                         
                // Llama programa de servicio pais                                                  
                Pgmpais(W_PaisControl:W_NombrePais);                                                
                Ppais = 0;                                                                          
                If W_PaisControl <> *Blanks;                                                        
                  p_cia = W_PaisControl;                                                            
                  OcultarCmp=*on;                                                                   
                  PosicionarZ=*on;                                                                  
                  clear pzona;                                                                      
                  clear plider;                                                                     
                Else;                                                                               
                  SflDsp = *Off;                                                                    
                  OcultarCmp=*off;                                                                  
                EndIf;                                                                              
              EndIf;                                                                                
                                                                                                    
                                                                                                    
              If W_PaisControl <> *Blanks;                                                          
                 If pzona <> *blanks;                                                               
                    PosicionarZ=*off;                                                               
                    Clear plider;                                                                   
                    Wsql  = 'Select Smname From Opf' +                                              
                     %Trim(p_cia) + '/PslMan ' +                                                    
                    'Where SmsLsp = '+ %Trim(PZona);                                                
                                                                                                    
                    Exec Sql                                                                        
                     Prepare C0 From :Wsql;                                                         
                                                                                                    
                    Exec Sql                                                                        
                     Declare Cursor1 Cursor For C0;                                                 
                                                                                                    
                    Exec Sql                                                                        
                     Open Cursor1;                                                                  
                                                                                                    
                    Exec Sql                                                                        
                     fetch Cursor1 Into :plider;                                                    
                                                                                                    
                    Exec Sql                                                                        
                     close Cursor1;                                                                 
                 EndIf;                                                                             
                                                                                                    
                 If plider <> '' Or Wind = 'R';                                                     
                    PintarFile();                                                                   
                 Else;                                                                              
                    PosicionarZ=*on;                                                                
                    Clear RControl1;                                                                
                    OcultarCmp=*on;                                                                 
                    PosicionarZ=*on;                                                                
                 EndIf;                                                                             
              Else;                                                                                 
               PosicionarZ = *on;                                                                   
              EndIf;                                                                                
            EndIf;                                                                                  
          EndDo;                                                                                    
       EndIf;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento para pintar el subfile principal                   //                       
       //------------------------------------------------------------------//                       
     PPintarfile       B                                                                            
     Dpintarfile       Pi                                                                           
      /Free                                                                                         
       Borrarfile();                                                                                
       If PosiX > *Zeros;                                                                           
          Posic = PosiX;                                                                            
          PosiX = *Zeros;                                                                           
       EndIf;                                                                                       
       If wind = 'M';                                                                               
         //campaña operativa                                                                        
         pgmcampo(W_PaisControl:pcame);                                                             
         pcamr =*blanks;                                                                            
         poper='+';                                                                                 
         pnume='001';                                                                               
         pgmcampa(p_cia:pcame:pcamr:poper:pnume);                                                   
         Nrr = Nrr + 1;                                                                             
         Posi = nrr;                                                                                
         scampa = pcamr;                                                                            
         pcame = pcamr;                                                                             
         pdescr = 'Meta para C'+ scampa;                                                            
         Write RDatos1;                                                                             
                                                                                                    
         For i=1 to 5;                                                                              
            pcamr =*blanks;                                                                         
            poper='-';                                                                              
            pnume='001';                                                                            
            pgmcampa(p_cia:pcame:pcamr:poper:pnume);                                                
            If Pcamr >= '1615';                                                                     
            Nrr = Nrr + 1;                                                                          
            pop = *Blanks;                                                                          
            Posi = nrr;                                                                             
            scampa = pcamr;                                                                         
            pdescr = 'Meta para C'+ scampa;                                                         
            pcame = pcamr;                                                                          
            Write RDatos1;                                                                          
           Endif;                                                                                   
         EndFor;                                                                                    
       ElseIf wind ='R';                                                                            
         Exec SQL                                                                                   
          select Max(estcam) into :pcame                                                            
          from orderp00/opiestf1                                                                    
          where estcia = :p_cia                                                                     
            and estest = 'S';                                                                       
                                                                                                    
         If pcame >= '1615' And pcame <> '2009';                                                    
          Pop = *Blanks;                                                                            
          Nrr = Nrr + 1;                                                                            
          Posi = nrr;                                                                               
          scampa = pcame;                                                                           
          pdescr = 'Cierre a C'+ scampa;                                                            
          Write RDatos1;                                                                            
         Endif;                                                                                     
                                                                                                    
         For i=1 to 5;                                                                              
            Pop = *Blanks;                                                                          
            pcamr =*blanks;                                                                         
            poper='-';                                                                              
            pnume='001';                                                                            
            pgmcampa(p_cia:pcame:pcamr:poper:pnume);                                                
            If Pcamr >= '1615' And Pcamr <> '2009';                                                 
             Nrr = Nrr + 1;                                                                         
             Posi = nrr;                                                                            
             scampa = pcamr;                                                                        
             pdescr = 'Cierre a C'+ scampa;                                                         
             //pcame = pcamr;                                                                       
             Write RDatos1;                                                                         
            Endif;                                                                                  
            pcame = pcamr;                                                                          
         EndFor;                                                                                    
       EndIf;                                                                                       
                                                                                                    
       SflEnd = *On;                                                                                
       If Nrr > *Zeros;                                                                             
         SflDsp = *On;                                                                              
       Else;                                                                                        
         SflDsp = *Off;                                                                             
       Endif;                                                                                       
      /End-Free                                                                                     
     PPintarfile       E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que se Encarga de Borrar Archivo de Control        //                       
       //------------------------------------------------------------------//                       
     PBorrarfile       B                                                                            
     DBorrarfile       Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write RControl1;                                                                             
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee las Opciones de la Pantalla                //                       
       //------------------------------------------------------------------//                       
     PLeerOpcion       B                                                                            
     DLeerOpcion       Pi                                                                           
      /Free                                                                                         
       If Nrr > *Zeros;                                                                             
         Readc Rdatos1;                                                                             
         Dow Not %Eof();                                                                            
           If Pop = 'X';                                                                            
              wnombre = plider;                                                                     
              Select;                                                                               
               When wind = 'M';                                                                     
                 PgmMetas(P_Cia:Scampa:Pzona:wnombre);                                              
               When wind = 'R';                                                                     
                 PgmResultados(P_Cia:Scampa);                                                       
              EndSl;                                                                                
              Posix = Posi;                                                                         
           EndIf;                                                                                   
           Pop = *Blanks;                                                                           
           Update Rdatos1;                                                                          
          Readc Rdatos1;                                                                            
         Enddo;                                                                                     
       Endif;                                                                                       
      /End-Free                                                                                     
     PLeerOpcion       E                                                                            
                                                                                                    
