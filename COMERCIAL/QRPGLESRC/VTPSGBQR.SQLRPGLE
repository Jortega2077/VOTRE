      **********************************************************************                        
      ** Programa     : VTPSGBQR                                          **                        
      ** Descripciòn: : Consulta Detalle x Sector - Resultados por Campaña**                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez- PersonalSoft        **                        
      ** Fecha Creaciòn: 07 de Junio de 2016                              **                        
      **********************************************************************                        
      ** Descripciòn: : Se agrega opción de consulta para retefuente      **                        
      ** Autor        : Leonel Mauricio Parra Suárez- PersonalSoft        **                        
      ** Responsable  : (JORTEGA)                                         **                        
      ** Fecha Creaciòn: 24 de Noviembre de 2017                          **                        
      **********************************************************************                        
      ** Descripciòn: : Solicitamos por favor eliminar el campo de        **                        
      **                Contactos efectivos plan básico y la opción F6    **                        
      **                para ver el detalle de los mismos.                **                        
      ** Autor        : Leonel Mauricio Parra Suárez- PersonalSoft        **                        
      ** Responsable  : (AParraQ)                                         **                        
      ** Fecha Creaciòn: 31 de Julio de 2018                              **                        
      **********************************************************************                        
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
                                                                                                    
     FVTPSGI6P  Cf   e             Workstn                                                          
     F                                     Sfile(RDatos1:Nrr)                                       
     F                                     InfDs( infDsp )                                          
                                                                                                    
      *Definición de Procedimientos                                                                 
     DMain             Pr                  Extpgm('Main')                                           
     DICia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIfoco                          30a                                                            
     DIpedbas                         5  0                                                          
     DImetpeda                        5  0                                                          
     DIpedtota                        5  0                                                          
     DIcummet                         2a                                                            
     DIpedpaga                        5  0                                                          
     DIinstota                        5  0                                                          
     DIinsdela                        9  0                                                          
     DIpeddela                        9  0                                                          
     DIalasa                         10a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DICia                            3a                                                            
     DIcamp                           4a                                                            
     DIzona                           3a                                                            
     DIsector                         1a                                                            
     DIcedulag                        9  0                                                          
     DInombreg                       30a                                                            
     DIfoco                          30a                                                            
     DIpedbas                         5  0                                                          
     DImetpeda                        5  0                                                          
     DIpedtota                        5  0                                                          
     DIcummet                         2a                                                            
     DIpedpaga                        5  0                                                          
     DIinstota                        5  0                                                          
     DIinsdela                        9  0                                                          
     DIpeddela                        9  0                                                          
     DIalasa                         10a                                                            
                                                                                                    
      *Definición de variables de trabajo                                                           
     DXcia             s             10a   Inz(*blanks)                                             
     DIdx              s              1  0 Inz(*Zeros)                                              
     DPtr              s               *   Inz(%Addr(*In))                                          
     DWsql             s            999a   Inz(*blanks)                                             
     DPcamr            s              4a   Inz(*blanks)                                             
     DPcame            s              4a   Inz(*blanks)                                             
     DPres             s              2a   Inz(*blanks)                                             
     Dkciar            s             10a   Inz(*blanks)                                             
     Dpoper            s              1a                                                            
     DWtitpanta0       s             60a   Inz(*blanks)                                             
     DWtitsec          s             60a   Inz(*blanks)                                             
     DWtitfoc          s             60a   Inz(*blanks)                                             
     DWtitced          s             60a   Inz(*blanks)                                             
     DWtitnom          s             60a   Inz(*blanks)                                             
     DWtitped          s             60a   Inz(*blanks)                                             
     DWtitmpea         s             60a   Inz(*blanks)                                             
     DWtitptoa         s             60a   Inz(*blanks)                                             
     DWtitcpea         s             60a   Inz(*blanks)                                             
     DWtitptpa         s             60a   Inz(*blanks)                                             
     DWtititoa         s             60a   Inz(*blanks)                                             
     DWtitidea         s             60a   Inz(*blanks)                                             
     DWtitpdea         s             60a   Inz(*blanks)                                             
     DWtitalaa         s             60a   Inz(*blanks)                                             
     DWtitcon          s             60a   Inz(*blanks)                                             
     DWtitvli          s             60a   Inz(*blanks)                                             
     DWtitcan          s             60a   Inz(*blanks)                                             
     DWtitvlr          s             60a   Inz(*blanks)                                             
     DWtittdc          s             60a   Inz(*blanks)                                             
     DWtitnot1         s            200a   Inz(*blanks)                                             
     DWtitnot2         s            200a   Inz(*blanks)                                             
     DWtitcef          s             60a   Inz(*blanks)                                             
     DWtitcam          s             60a   Inz(*blanks)                                             
     DWtitpde          s             60a   Inz(*blanks)                                             
     DWtitala          s             60a   Inz(*blanks)                                             
     DWtitcal          s             60a   Inz(*blanks)                                             
     DWbot01           s             30a   Inz(*blanks)                                             
     DWbot02           s             30a   Inz(*blanks)                                             
     DWbot03           s             30a   Inz(*blanks)                                             
     DWbot04           s             30a   Inz(*blanks)                                             
     DWbot05           s             30a   Inz(*blanks)                                             
     DWbot06           s             30a   Inz(*blanks)                                             
     DWbot07           s             30a   Inz(*blanks)                                             
     DWcampant         s              4a   Inz(*blanks)                                             
     DWdcto            s             15a   Inz(*blanks)                                             
     DWcefe            s              5  0 Inz(*ZEROS)                                              
     DWcam1            s              4a   Inz(*blanks)                                             
     DWcam2            s              4a   Inz(*blanks)                                             
     DWcam3            s              4a   Inz(*blanks)                                             
     DWpedd1           s              9  0 Inz(*ZEROS)                                              
     DWpedd2           s              9  0 Inz(*ZEROS)                                              
     DWpedd3           s              9  0 Inz(*ZEROS)                                              
     DWalas1           s             10a   Inz(*blanks)                                             
     DWalas2           s             10a   Inz(*blanks)                                             
     DWalas3           s             10a   Inz(*blanks)                                             
     DWstatus          s              1a   Inz(*blanks)                                             
     DWdestatus        s            120a   Inz(*blanks)                                             
     DWcia             s             10a   Inz(*Blanks)                                             
     DWzona            s              3a   Inz(*Blanks)                                             
     DWcamp            s              4a   Inz(*Blanks)                                             
     DYcia             s              3a   Inz(*Blanks)                                             
     DXcampa           s              4a   Inz(*Blanks)                                             
     DXzona            s              3a   Inz(*Blanks)                                             
     DXSector          s             10a   Inz(*Blanks)                                             
     DXCedula          s              9  0 Inz(*Zeros)                                              
     DXNombre          s             30a   Inz(*Blanks)                                             
     DXcefe            s              5  0 Inz(*ZEROS)                                              
     DXFOCO            s             30a   Inz(*Blanks)                                             
     DXPedidoB         s              5  0 Inz(*Zeros)                                              
     DXMetPedA         s              5  0 Inz(*Zeros)                                              
     DXPedTotA         s              5  0 Inz(*Zeros)                                              
     DXCumPedA         s              2a   Inz(*Blanks)                                             
     DXPedPagA         s              5  0 Inz(*ZEROS)                                              
     DXPedDelA         s              9  0 Inz(*Zeros)                                              
     DXCampAnt         s              4a   Inz(*Blanks)                                             
     DXinstot          s              5  0 Inz(*Zeros)                                              
     DXinsdel          s              9  0 Inz(*Zeros)                                              
     DPtitRtf1         s            100a   Inz(*Blanks)                                             
     DPtitRtf2         s            100a   Inz(*Blanks)                                             
     DPtitRtf3         s            100a   Inz(*Blanks)                                             
     DPtitRtf4         s            100a   Inz(*Blanks)                                             
     DPtitRtf5         s            100a   Inz(*Blanks)                                             
     DPtitRtf6         s            100a   Inz(*Blanks)                                             
     DPVlrRtf1         s             60a   Inz(*Blanks)                                             
     DPVlrRtf2         s             60a   Inz(*Blanks)                                             
     DPVlrRtf3         s             60a   Inz(*Blanks)                                             
     DPVlrRtf4         s             60a   Inz(*Blanks)                                             
     DPVlrRtf5         s             60a   Inz(*Blanks)                                             
     DPVlrRtf6         s             60a   Inz(*Blanks)                                             
                                                                                                    
                                                                                                    
     DRsTramaDetalle   s                   SqlType(Result_Set_Locator)                              
     ** Definiciòn de Estructuras de Trabajo                                                        
     DDsDetalleR       Ds                  Occurs(5)                                                
     DRConcepto                      50a                                                            
     DRValorInd                      15a                                                            
     DRCantidad                       5  0                                                          
     DRValor                         15a                                                            
                                                                                                    
     DIndic            Ds                  Based(Ptr)                                               
     DSalir                            N   Overlay(Indic:3)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
     DSflDsp                           N   Overlay(Indic:85)                                        
     DSflDspCtl                        N   Overlay(Indic:80)                                        
     DSflClr                           N   Overlay(Indic:75)                                        
     DSflEnd                           N   Overlay(Indic:70)                                        
     DDescuento                        N   Overlay(Indic:10)                                        
     DPedidos                          N   Overlay(Indic:5)                                         
     DInscripcns                       N   Overlay(Indic:6)                                         
     DPlanBasico                       N   Overlay(Indic:8)                                         
                                                                                                    
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
                                                                                                    
      *Definición de Estructuras                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPintarfile       Pr                                                                           
     DBorrarfile       Pr                                                                           
     DPrDescuento      Pr                                                                           
     DPrCentrar        Pr             6A                                                            
     D pCadena                        6A   Const                                                    
                                                                                                    
     ** Programas Externos                                                                          
     DPgmcompa         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSpcia                          10                                                             
     DPres                            2                                                             
     Dkciar                          10                                                             
                                                                                                    
     DPgmDllPedRtaxCÑ  Pr                  Extpgm('VOTREP00/VTPSGBRR')                              
     DYcia                            3a                                                            
     DXcampa                          4a                                                            
     DXzona                           3a                                                            
     DXSector                        10a                                                            
     DXCedula                         9  0                                                          
     DXNombre                        30a                                                            
     DXFOCO                          30a                                                            
     DXPedidoB                        5  0                                                          
     DXMetPedA                        5  0                                                          
     DXPedTotA                        5  0                                                          
     DXCumPedA                        2a                                                            
     DXPedPagA                        5  0                                                          
     DXPedDelA                        9  0                                                          
     DXCampAnt                        4a                                                            
      *Procedimiento que invoca PGM detalles de inscripciones                                       
     DPgmDllInscripns  Pr                  Extpgm('VOTREP00/VTPSGBTR')                              
     DYcia                            3a                                                            
     DXcampa                          4a                                                            
     DXzona                           3a                                                            
     DXSector                        10a                                                            
     DXCedula                         9  0                                                          
     DXNombre                        30a                                                            
     DXFOCO                          30a                                                            
     DXinstot                         5  0                                                          
     DXinsdel                         9  0                                                          
     DXCampAnt                        4a                                                            
      *Procedimiento que invoca PGM de Plan de Guías                                                
     DPgmPlanBasico    Pr                  Extpgm('VOTREP00/VTPSGBVR')                              
     DYcia                            3a                                                            
     DXcampa                          4a                                                            
     DXzona                           3a                                                            
     DXSector                        10a                                                            
     DXCedula                         9  0                                                          
     DXNombre                        30a                                                            
     DXFOCO                          30a                                                            
     DXCefe                           5  0                                                          
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //                                                                  //                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //                                                                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
                                                                                                    
       Dow Salir = *Off;                                                                            
        Clear *all DsDetalleR;                                                                      
        Clear WCIA;                                                                                 
        Clear WZONA;                                                                                
        Clear WCAMP;                                                                                
        Clear kciar;                                                                                
        Clear PRes;                                                                                 
                                                                                                    
         Xcia = *Blanks;                                                                            
         Xcia =%Trim(Icia);                                                                         
         pgmcompa(Xcia:PRes:kciar);                                                                 
       WCIA  =   kciar;                                                                             
       WZONA =   Izona;                                                                             
       WCAMP =   Icamp;                                                                             
         If PRes='Si';                                                                              
            Exec SQL                                                                                
             CALL VOTREP00.GSL_DETALLEDCTOGUIAS(:Wcia,                                              
             :Wzona,:Wcamp,:Isector,:Icedulag,:Inombreg,:Ifoco,:Ipedbas,                            
             :Imetpeda,:Ipedtota,:Icummet,:Ipedpaga,:Iinstota,:Iinsdela,                            
             :Ipeddela,:Ialasa,:Wtitpanta0,:Wtitsec,:Wtitfoc,:Wtitced,                              
             :Wtitnom,:Wtitped,:Wtitmpea,:Wtitptoa,:Wtitcpea,:Wtitptpa,                             
             :Wtititoa,:Wtitidea,:Wtitpdea,:Wtitalaa,:Wtitcon,:Wtitvli,                             
             :Wtitcan,:Wtitvlr,:Wtittdc,:Wtitnot1,:Wtitnot2,:Wtitcef,                               
             :Wtitcam,:Wtitpde,:Wtitala,:Wtitcal,:Wbot01,:Wbot02,:Wbot03,                           
             :Wbot04,:Wbot05,:Wbot06,:Wbot06,:Wcampant,:Wdcto,:Wcefe,:Wcam1,                        
             :Wcam2,:Wcam3,:Wpedd1,:Wpedd2,:Wpedd3,:Walas1,:Walas2,:Walas3,                         
             :Wstatus,:Wdestatus,                                                                   
             :PtitRtf1,:PtitRtf2,:PtitRtf3,:PtitRtf4,:PtitRtf5,:PtitRtf6,                           
             :PVlrRtf1,:PVlrRtf2,:PVlrRtf3,:PVlrRtf4,:PVlrRtf5,:PVlrRtf6);                          
            Exec SQL                                                                                
             Associate Result Set Locators                                                          
             (:RsTramaDetalle) With Procedure VOTREP00.GSL_DETALLEDCTOGUIAS;                        
                                                                                                    
            Exec SQL                                                                                
                   Allocate Resultados Cursor                                                       
                   For Result Set :RsTramaDetalle;                                                  
                                                                                                    
            Exec SQL                                                                                
                   Fetch Resultados For 5 Rows Into :DsDetalleR;                                    
                                                                                                    
            Exec SQL                                                                                
                   Close Resultados;                                                                
                                                                                                    
            Pfecha = %Char(%Date());                                                                
            Ppgm = Nomp;                                                                            
                                                                                                    
            PintarFile();                                                                           
            Write Teclas1;                                                                          
            SflDspCtl = *On;                                                                        
            Exfmt RControl1;                                                                        
             If Descuento = *on;                                                                    
               PrDescuento();                                                                       
               Descuento = *off;                                                                    
             Endif;                                                                                 
             If Pedidos = *on;                                                                      
               PrPedidos();                                                                         
                Pedidos = *off;                                                                     
             Endif;                                                                                 
             If Inscripcns = *on;                                                                   
               PrInscripcions();                                                                    
                Inscripcns = *off;                                                                  
             Endif;                                                                                 
             If PlanBasico = *on;                                                                   
               PrPlanBasico();                                                                      
                PlanBasico = *off;                                                                  
             Endif;                                                                                 
            SflDspCtl = *Off;                                                                       
            If Salir = *On;                                                                         
               Leave;                                                                               
            EndIf;                                                                                  
         Endif;                                                                                     
               Clear RControl1;                                                                     
               Clear Teclas1;                                                                       
       EndDo;                                                                                       
                                                                                                    
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //        Procedimiento para pintar el subfile principal            //                        
      //////////////////////////////////////////////////////////////////////                        
     PPintarfile       B                                                                            
     Dpintarfile       Pi                                                                           
      /Free                                                                                         
                                                                                                    
       Borrarfile();                                                                                
       PrCabecera();                                                                                
       For Idx=1 to %elem(DsDetalleR);                                                              
           %Occur(DsDetalleR) = Idx;                                                                
            Nrr = Nrr + 1;                                                                          
            PNOMBR = RConcepto;                                                                     
      *     PNOMBR = DsDetalleR.RConcepto;                                                          
            EvalR PVALINDV = %Trim(RValorInd);                                                      
      *     EvalR PVALINDV = %Trim(DsDetalleR.RValorInd);                                           
            PCANT = RCantidad;                                                                      
      *     PCANT = DsDetalleR.RCantidad;                                                           
            EvalR PVALORT = %Trim(RValor);                                                          
      *     EvalR PVALORT = %Trim(DsDetalleR.RValor);                                               
            Write RDatos1;                                                                          
       Endfor;                                                                                      
                                                                                                    
       SflEnd = *On;                                                                                
       If Nrr > *Zeros;                                                                             
         SflDsp = *On;                                                                              
       Else;                                                                                        
         SflDsp = *Off;                                                                             
       EndIf;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPintarfile       E                                                                            
                                                                                                    
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Borrar Archivo de Control        //                       
       //////////////////////////////////////////////////////////////////////                       
     PBorrarfile       B                                                                            
     DBorrarfile       Pi                                                                           
      /Free                                                                                         
                                                                                                    
       Nrr = *Zeros;                                                                                
       SflClr = *On;                                                                                
       Write RControl1;                                                                             
       SflClr = *Off;                                                                               
                                                                                                    
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Asignar Campos de Cabecera       //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrCabecera       B                                                                            
     DPrCabecera       Pi                                                                           
      /Free                                                                                         
                                                                                                    
          PCAMPA = 'Cierre C' + WCAMP;                                                              
          PSECTR = Isector;                                                                         
          PZNA   = Wzona;                                                                           
          PCEDUL = Icedulag;                                                                        
          PFOCO  = Ifoco;                                                                           
          PNOMB  = Inombreg;                                                                        
          If Ipedbas = *Zeros;                                                                      
          PPEDBAS= '#0';                                                                            
          Else;                                                                                     
          PPEDBAS= '#'+%Trim(%Editc(Ipedbas:'Z'));                                                  
          EndIf;                                                                                    
          If Iinstota = *Zeros;                                                                     
          PINSCR = '#0';                                                                            
          Else;                                                                                     
          PINSCR = '#'+%Trim(%Editc(Iinstota:'Z'));                                                 
          EndIf;                                                                                    
          PCÑA2  = 'C'+ Wcampant;                                                                   
          If Imetpeda = *Zeros;                                                                     
           PMTAPED= '#0';                                                                           
          Else;                                                                                     
          PMTAPED= '#'+%Trim(%Editc(Imetpeda:'Z'));                                                 
          EndIf;                                                                                    
          PCÑA4  = 'C'+ Wcampant;                                                                   
          If Ipeddela = *Zeros;                                                                     
           PPEDDEL= '#0';                                                                           
          Else;                                                                                     
          PPEDDEL= '#'+%Trim(%Editc(Ipeddela:'Z'));                                                 
          EndIf;                                                                                    
          If Iinsdela = *Zeros;                                                                     
           PINSDEL= '#0';                                                                           
          Else;                                                                                     
           PINSDEL= '#'+%Trim(%Editc(Iinsdela:'Z'));                                                
          EndIf;                                                                                    
          PCÑA3  = 'C'+ Wcampant;                                                                   
          If Ipedtota = *Zeros;                                                                     
           PPEDTTL= '#0';                                                                           
          Else;                                                                                     
           PPEDTTL= '#'+%Trim(%Editc(Ipedtota:'Z'));                                                
          EndIf;                                                                                    
          PCÑA5  = 'C'+ Wcampant;                                                                   
          PCUMMET = Icummet;                                                                        
          PCÑA6  = 'C'+ Wcampant;                                                                   
          PNVALAS = Ialasa;                                                                         
          If Ipedpaga = *Zeros;                                                                     
           PPEDTTPG= '#0';                                                                          
          Else;                                                                                     
           PPEDTTPG='#'+%Trim(%Editc(Ipedpaga:'Z'));                                                
          EndIf;                                                                                    
           PCAM1 =  PrCentrar(Wcam1);                                                               
           PCAM2 =  PrCentrar(Wcam2);                                                               
           PCAM3 =  PrCentrar(Wcam3);                                                               
           PPEDD1 = PrCentrar(%Trim(%Editc(Wpedd1:'Z')));                                           
           PPEDD2 = PrCentrar(%Trim(%Editc(Wpedd2:'Z')));                                           
           PPEDD3 = PrCentrar(%Trim(%Editc(Wpedd3:'Z')));                                           
           PALAS1 = PrCentrar(Walas1);                                                              
           PALAS2 = PrCentrar(Walas2);                                                              
           PALAS3 = PrCentrar(Walas3);                                                              
           PCANBAL= Wtitcal;                                                                        
                                                                                                    
           If %Scan('SI':Wtitcal) > 0;                                                              
              PCMBALAS = 'SI';                                                                      
           ElseIf %Scan('NO':Wtitcal) > 0;                                                          
              PCMBALAS = 'NO';                                                                      
           EndIf;                                                                                   
           //PCMBALAS = %Subst(Wtitcal:22:23);                                                      
           //PTITCEF = WTITCEF;                                                                     
           //PCEFE = '#' + %Trim(%Editc(WCEFE:'Z'));                                                
      /End-Free                                                                                     
     PPrCabecera       E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Ejecutar Ventana de Descuentos   //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrDescuento      B                                                                            
     DPrDescuento      Pi                                                                           
      /Free                                                                                         
                                                                                                    
           PTITTDC  = WTITTDC;                                                                      
           PDCTO    = WDCTO;                                                                        
           Ptitnot1 = %Subst(Wtitnot1:1:60);                                                        
           Ptitnot1a= %Subst(Wtitnot1:61:60);                                                       
           Ptitnot2 = %Subst(Wtitnot2:1:60);                                                        
           Ptitnot2a= %Subst(Wtitnot2:61:60);                                                       
           Ptitnot2b= %Subst(Wtitnot2:121:60);                                                      
           PDesrete = %Trim(PVlrRtf3);                                                              
           Exfmt Descuentos;                                                                        
           If PSelec = 1;                                                                           
              Clear PSelec;                                                                         
                  Dow Salir = *Off;                                                                 
                      Clear *all RDato20;                                                           
                      PTITU3  ='  Detalle Retención';                                               
                      PVAL1   = %Trim(PtitRtf1);                                                    
                      PVAL2   = %Trim(PtitRtf2);                                                    
                      PVAL3   = %Trim(PtitRtf3);                                                    
                      PVAL4   = %Trim(PtitRtf4);                                                    
                      PVAL5   = %Trim(PtitRtf5);                                                    
                      Evalr PVALUE1 = %Trim(PVlrRtf1);                                              
                      Evalr PVALUE2 = %Trim(PVlrRtf2);                                              
                      Evalr PVALUE3 = %Trim(PVlrRtf3);                                              
                      Evalr PVALUE4 = %Trim(PVlrRtf4);                                              
                      Evalr PVALUE5 = %Trim(PVlrRtf5);                                              
                      Exfmt RDato20;                                                                
                 EndDo;                                                                             
                 Salir = *Off;                                                                      
           EndIf;                                                                                   
      /End-Free                                                                                     
     PPrDescuento      E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Invocar SP de pedidos            //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrPedidos        B                                                                            
     DPrPedidos        Pi                                                                           
      /Free                                                                                         
              Ycia     = ICIA;                                                                      
              Xcampa   = Wcamp;                                                                     
              Xzona    = WZONA;                                                                     
              XSector  = Isector;                                                                   
              XCedula  = Icedulag;                                                                  
              XNombre  = Inombreg;                                                                  
              XFOCO    = Ifoco;                                                                     
              XPedidoB = Ipedbas;                                                                   
              XMetPedA = Imetpeda;                                                                  
              XPedTotA = Ipedtota;                                                                  
              XCumPedA = Icummet;                                                                   
              XPedPagA = Ipedpaga;                                                                  
              XPedDelA = Ipeddela;                                                                  
              XCampAnt = Wcampant;                                                                  
        PgmDllPedRtaxCÑ(Ycia:Xcampa:XZONA:XSector:XCedula:XNombre                                   
              :Xfoco:XPedidoB:XMetPedA:XPedTotA:XCumPedA:XPedPagA                                   
              :XPedDelA:XCampAnt);                                                                  
      /End-Free                                                                                     
     PPrPedidos        E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Invocar SP de Inscripciones      //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrInscripcions   B                                                                            
     DPrInscripcions   Pi                                                                           
      /Free                                                                                         
              Ycia     = ICIA;                                                                      
              Xcampa   = Wcamp;                                                                     
              Xzona    = WZONA;                                                                     
              XSector  = Isector;                                                                   
              XCedula  = Icedulag;                                                                  
              XNombre  = Inombreg;                                                                  
              XFOCO    = Ifoco;                                                                     
              Xinstot  = Iinstota;                                                                  
              Xinsdel  = Iinsdela;                                                                  
              XCampAnt = Wcampant;                                                                  
        PgmDllInscripns(Ycia:Xcampa:XZONA:XSector:XCedula:XNombre                                   
              :Xfoco:Xinstot:Xinsdel:XCampAnt);                                                     
                                                                                                    
      /End-Free                                                                                     
     PPrInscripcions   E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Invocar SP de Plan Basico        //                       
       //////////////////////////////////////////////////////////////////////                       
     PPrPlanBasico     B                                                                            
     DPrPlanBasico     Pi                                                                           
      /Free                                                                                         
              Ycia     = ICIA;                                                                      
              Xcampa   = Wcamp;                                                                     
              Xzona    = WZONA;                                                                     
              XSector  = Isector;                                                                   
              XCedula  = Icedulag;                                                                  
              XNombre  = Inombreg;                                                                  
              XFOCO    = Ifoco;                                                                     
              XCefe    = WCefe;                                                                     
        PgmPlanBasico(Ycia:Xcampa:XZONA:XSector:XCedula:XNombre                                     
              :Xfoco:XCefe);                                                                        
                                                                                                    
      /End-Free                                                                                     
     PPrPlanBasico     E                                                                            
                                                                                                    
                                                                                                    
      *------------------------------------------------------------------------                     
      * Centrar cadena alfanumérica                                                                 
      *------------------------------------------------------------------------                     
     PPrCentrar        B                                                                            
     D                 Pi             6A                                                            
     D pCadena                        6A   Const                                                    
                                                                                                    
      *- Variables                                                                                  
     DW_Blanco         S              5S 0                                                          
     DW_Inicio         S              5S 0 Inz(*Zeros)                                              
     DW_Longit         S              5S 0 Inz(*Zeros)                                              
     DW_TxtCen         S              6A   Inz(*Blanks)                                             
      /Free                                                                                         
       //Validar mensaje                                                                            
         If (pCadena = *Blanks) Or                                                                  
            (%Len(%Trim(pCadena)) = 6);                                                             
             Return pCadena;                                                                        
         EndIf;                                                                                     
                                                                                                    
       //Calcular longitud de Texto y ubicarlo en el centro                                         
         W_Blanco = %Len(pCadena) - %Len(%Trim(pCadena));                                           
         W_Inicio = (W_Blanco/2) + 1;                                                               
         W_Longit = %Len(%Trim(pCadena));                                                           
         %SubSt(W_TxtCen:W_Inicio:W_Longit) = %Trim(pCadena);                                       
       //Devolver Texto Centrado                                                                    
         Return W_TxtCen;                                                                           
      /End-Free                                                                                     
     PPrCentrar        E                                                                            
