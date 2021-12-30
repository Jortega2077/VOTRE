      **********************************************************************                        
      ** Programa     : VTPSGBVR                                          **                        
      ** Descripciòn: : Web Guías/ Plan Básico -  Resultado por Campaña   **                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 13 de Julio de 2016                              **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
      ** Definición de Archivos de Trabajo                                                          
     FLcacmp01  If   e           k Disk                                                             
                                                                                                    
     FVTPSGI9P  cf   e             Workstn                                                          
     F                                     Sfile(RDatos:Nrr)                                        
     F                                      InfDs( infDsp )                                         
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
      *Definición de Procedimientos                                                                 
     DMain             Pr                  Extpgm('VTPSGBVR')                                       
     DIcia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIFoco                          30a                                                            
     DIcefe                           5  0                                                          
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIFoco                          30a                                                            
     DIcefe                           5  0                                                          
                                                                                                    
      ** Definición de Variables de Trabajo                                                         
     DPtr              s               *   Inz(%Addr(*In))                                          
     DPosiX            s              5  0 Inz(*Zeros)                                              
     DSPcia            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPciaR            s             10a   Inz(*Blanks)                                             
     DIDX              s              4  0 Inz(*Zeros)                                              
     DWcia             s             10a   Inz(*Blanks)                                             
     DWzona            s              3a   Inz(*Blanks)                                             
     DWcamp            s              4a   Inz(*Blanks)                                             
     DWtitpanta0       s             60a   Inz(*Blanks)                                             
     DWtitcedg         s             60a   Inz(*Blanks)                                             
     DWtitnomg         s             60a   Inz(*Blanks)                                             
     DWtitcef          s             60a   Inz(*Blanks)                                             
     DWtitcedc         s             60a   Inz(*Blanks)                                             
     DWtitnomc         s             60a   Inz(*Blanks)                                             
     DWtitcapc         s             60a   Inz(*Blanks)                                             
     DWtitcaec         s             60a   Inz(*Blanks)                                             
     DWtitprec         s             60a   Inz(*Blanks)                                             
     DWtitfecc         s             60a   Inz(*Blanks)                                             
     DWtitenvc         s             60a   Inz(*Blanks)                                             
     DWbot01           s             30a   Inz(*Blanks)                                             
     DWbot02           s             30a   Inz(*Blanks)                                             
     DWstatus          s              1a   Inz(*Blanks)                                             
     DWdestatus        s            120a   Inz(*Blanks)                                             
     DInd_Ref          s               N   Inz(*Off)                                                
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DSflEnd                           N   Overlay(Indic:40)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DOcultarOp                        N   Overlay(Indic:30)                                        
     DMostrarDet                       N   Overlay(Indic:31)                                        
                                                                                                    
      ** Definición de Procedimientos                                                               
     DPrLlenarSfl      Pr                                                                           
     DPrBorrarSfl      Pr                                                                           
     DPrAsigCabec      pr                                                                           
     DPrLeerOpcion     pr                                                                           
                                                                                                    
      *Procedimiento Recupera Código de envio                                                       
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10                                                             
     DPres                            2                                                             
     DKciaR                          10                                                             
                                                                                                    
     DRsTramaContact   s                   SqlType(Result_Set_Locator)                              
                                                                                                    
      *Definición de Occurs para cargue de Información                                              
     DDsContactos      Ds                  Occurs(999)                                              
     DGCedula                         9  0                                                          
     DGNombre                        30a                                                            
     DGCampaPed                       4a                                                            
     DGCampaEfe                       4a                                                            
     DGPremio                        40a                                                            
     DGFechaDesc                     10a                                                            
     DGEnvioPre                      40a                                                            
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
                                                                                                    
      /Free                                                                                         
       Dow Not Salir;                                                                               
          clear *all DsContactos;                                                                   
          SPcia = ICia;                                                                             
          Pres = *Blanks;                                                                           
          PciaR = *Blanks;                                                                          
          CompaÑia(SPcia:Pres:PciaR);                                                               
          WCia  =   PciaR;                                                                          
          WZONA =   Izona;                                                                          
          WCAMP =   Icamp;                                                                          
          If Pres= 'Si';                                                                            
             Exec SQL                                                                               
              Call Votrep00.Gsl_Contactosdctoguias (:WCia,:Wzona,                                   
              :Wcamp,:Isector,:Icedulag,:Inombreg,:Icefe,                                           
              :Wtitpanta0,:Wtitcedg,:Wtitnomg,:Wtitcef,                                             
              :Wtitcedc,:Wtitnomc,:Wtitcapc,:Wtitcaec,:Wtitprec,:Wtitfecc,                          
              :Wtitenvc,:Wbot01,:Wbot02,:Wstatus,:Wdestatus);                                       
                                                                                                    
             Exec SQL                                                                               
              Associate Result Set Locators (:RsTramaContact)                                       
              With Procedure Votrep00/GSL_ContactosDctoGuias;                                       
                                                                                                    
             Exec SQL                                                                               
              Allocate Resultados Cursor                                                            
              For Result Set :RsTramaContact;                                                       
                                                                                                    
             Exec SQL                                                                               
              Fetch Resultados For 100 Rows Into :DsContactos;                                      
                                                                                                    
             Exec SQL                                                                               
              Close Resultados;                                                                     
                                                                                                    
             PrLlenarSfl();                                                                         
             PrPintarSfl();                                                                         
             If Salir;                                                                              
                Leave;                                                                              
             EndIf;                                                                                 
             If Nrr > *Zeros;                                                                       
              PrLeerOpcion();                                                                       
            Endif;                                                                                  
          EndIf;                                                                                    
       Enddo;                                                                                       
       *InLr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrBorrarSfl      B                                                                            
     DPrBorrarSfl      Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write RControl;                                                                              
       SflClr = *Off;                                                                               
      /End-Free                                                                                     
     PPrBorrarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrLlenarSfl      B                                                                            
     DPrLlenarSfl      Pi                                                                           
      /Free                                                                                         
       PrBorrarSfl();                                                                               
       If PosiX <> *Zeros;                                                                          
          Posic = PosiX;                                                                            
          PosiX = *Zeros;                                                                           
       EndIf;                                                                                       
       PrAsigCabec();                                                                               
       MostrarDet = *Off;                                                                           
                                                                                                    
       For idx = 1 to %Elem(DsContactos);                                                           
        %Occur(DsContactos)= Idx;                                                                   
        If DsContactos.GNombre <> *Blanks Or DsContactos.GCampaPed <> *Blanks                       
           Or DsContactos.GCedula <> *Zeros Or DsContactos.GCampaEfe <> *Blanks                     
           Or DsContactos.GPremio <> *Blanks;                                                       
           Nrr = Nrr +1;                                                                            
           OcultarOp = *Off;                                                                        
           PCD      = DsContactos.GCedula;                                                          
           PNOMBRE  = DsContactos.GNombre;                                                          
           PCÑ1PED  = DsContactos.GCampaPed;                                                        
           PCÑEFE   = DsContactos.GCampaEfe;                                                        
           PPREMIO  = DsContactos.GPremio;                                                          
           PFECHA   = DsContactos.GFechaDesc;                                                       
           PENVIO   = DsContactos.GEnvioPre;                                                        
           If PENVIO = *Blanks;                                                                     
              OcultarOp = *On;                                                                      
           Else;                                                                                    
              Ind_Ref = *On;                                                                        
           EndIf;                                                                                   
           Posi = Nrr;                                                                              
           Write RDatos;                                                                            
        EndIf;                                                                                      
       EndFor;                                                                                      
                                                                                                    
       If Ind_Ref = *On;                                                                            
          MostrarDet = *On;                                                                         
       EndIf;                                                                                       
                                                                                                    
       SflEnd = *On;                                                                                
       If Nrr > *Zeros;                                                                             
          SflDsp = *On;                                                                             
       Else;                                                                                        
          SflDsp = *Off;                                                                            
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrLlenarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Llena el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrAsigCabec      B                                                                            
     DPrAsigCabec      Pi                                                                           
      /Free                                                                                         
        Pzona   = Wzona;                                                                            
        PCEDUG  = Icedulag;                                                                         
        PNOMBG  = Inombreg;                                                                         
        PCÑA1   = ICamp;                                                                            
                                                                                                    
        If Icefe = *Zeros;                                                                          
           PCEPB = '#0';                                                                            
        Else;                                                                                       
           PCEPB = '#' + %Trim(%editc(Icefe:'Z'));                                                  
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrAsigCabec      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrPintarSfl      B                                                                            
     DPrPintarSfl      Pi                                                                           
      /Free                                                                                         
       Ppgm = Nomp;                                                                                 
       Pfecha = %Char(%Date());                                                                     
       SflDspCtl = *On;                                                                             
       Write RTeclas;                                                                               
       Exfmt RControl;                                                                              
       SflDspCtl = *Off;                                                                            
      /End-Free                                                                                     
     PPrPintarSfl      E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Lee las Opciones de la Pantalla                //                       
       //------------------------------------------------------------------//                       
     PPrLeerOpcion     B                                                                            
     DPrLeerOpcion     Pi                                                                           
      /Free                                                                                         
       If Nrr > *Zeros;                                                                             
         Readc Rdatos;                                                                              
         Dow Not %Eof();                                                                            
           If Pop = '5';                                                                            
              Clear RDetalle;                                                                       
              PDEnvio = PEnvio;                                                                     
              Exfmt RDetalle;                                                                       
              Posix = Posi;                                                                         
           EndIf;                                                                                   
           Pop = *Blanks;                                                                           
           Update Rdatos;                                                                           
           Readc Rdatos;                                                                            
         EndDo;                                                                                     
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrLeerOpcion     E                                                                            
