      ***********************************************************************                       
      ** Programa      : VTPSGC0R                                          **                       
      ** Descripciòn   : PGM Consulta Control de Pagos Guías               **                       
      ** Proyecto      : Plan de Guías                                     **                       
      ** Autor         : Leonel Mauricio Parra Suárez-PersonalSoft S.A.    **                       
      ** Fecha Creación: 03 de Abril de 2017                               **                       
      **                                                                                            
      ***********************************************************************                       
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
     Hdatfmt(*Iso)                                                                                  
                                                                                                    
     **Definicion de informes                                                                       
     FVTPSGC0I  o    e             Printer Oflind(*In99) Usropn                                     
     FVTPSGC0P  Cf   e             Workstn                                                          
     F                                     InfDs( infDsp )                                          
      **Definición de Variables de Trabajo                                                          
     DUser             s             10a   Inz(*User)                                               
     D$C               c                   ''''                                                     
     DXInd             s              1A                                                            
     DSqlStr           s           3000a   Inz(*Blanks)                                             
     Dwpais            s             30                                                             
     DICam             s              4                                                             
     DNumZon           s              3  0 Inz(*Zeros)                                              
     Drespuesta        s             30                                                             
     D dx1             s              3  0 Inz(*Zeros)                                              
     D dx2             s              3  0 Inz(*Zeros)                                              
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DSPcia            s             10a   Inz(*Blanks)                                             
     DPres             s              2a   Inz(*Blanks)                                             
     DPciaR            s             10a   Inz(*Blanks)                                             
     DIDX              s              5  0 Inz(*Zeros)                                              
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
     D w_AcuCar        s             15  2 Inz(*Zeros)                                              
     D w_AcuCarP       s             15  2 Inz(*Zeros)                                              
     D w_AcuEfe        s             15  2 Inz(*Zeros)                                              
     DWADTNDÑ          s             30                                                             
     Dw_ZNA            s              3                                                             
     DCnades           s             20    Inz(*Blanks)                                             
     DTced             s             30    Inz(*Blanks)                                             
     DTcns             s              9    Inz(*Blanks)                                             
     DTced15           s             15  0 Inz(*Zeros)                                              
     DW_IndPagoBanco   s              1    Inz(*Blanks)                                             
      ** Definición de Variables de Trabajo                                                         
     DCmd              s            500                                                             
     DLong             s             15p 5                                                          
     DKzona            s              3a   Inz(' ')                                                 
     DWmotivo          s            200a   Inz(' ')                                                 
     DZLpgtds          s             11  2                                                          
     DZLpgvdc          s             11  2                                                          
     DIndPro           s              1a                                                            
                                                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
      *                                                                                             
     DFields         e Ds                  Extname(VtwLpgf:RegLpg)                                  
     D                                     Prefix(Q)                                                
     DFields1        e Ds                  Extname(Pardtl:PARDTLF1)                                 
     D                                     Prefix(Q)                                                
      *                                                                                             
     DRsTramaResult    s                   SqlType(Result_Set_Locator)                              
     DRsTramaDetalle   s                   SqlType(Result_Set_Locator)                              
      *Definición de Occurs para cargue de Información                                              
     DDsResultadosGu   Ds                                                                           
     DWtitced                        60a   Inz(*Blanks)                                             
     DWtitsec                        60a   Inz(*Blanks)                                             
     DWnrosec                         2a   Inz(*Blanks)                                             
     DWfoco                          30a   Inz(*Blanks)                                             
     DWtitpanta1                     60a   Inz(*Blanks)                                             
     DWtitpanta2                     60a   Inz(*Blanks)                                             
     DWtitpanta3                     60a   Inz(*Blanks)                                             
     DWtitnom                        60a   Inz(*Blanks)                                             
     DWtitped                        60a   Inz(*Blanks)                                             
     DWtitmpta                       60a   Inz(*Blanks)                                             
     DWtitptoa                       60a   Inz(*Blanks)                                             
     DWtitcpea                       60a   Inz(*Blanks)                                             
     DWtitmpe                        60a   Inz(*Blanks)                                             
     DWtitpto                        60a   Inz(*Blanks)                                             
     DWtitcpe                        60a   Inz(*Blanks)                                             
     DWtitito                        60a   Inz(*Blanks)                                             
     DDsDetalleGuias   Ds                                                                           
     DWcia                           10a   Inz(*Blanks)                                             
     DWzona                           3a   Inz(*Blanks)                                             
     DWzonaC                          3a   Inz(*Blanks)                                             
     DWcamp                           4a   Inz(*Blanks)                                             
     DIsector                         1a                                                            
     DIsectorC                        1a                                                            
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
     DWtitpanta0                     60a   Inz(*blanks)                                             
     DWtitfoc                        60a   Inz(*blanks)                                             
     DWtitmpea                       60a   Inz(*blanks)                                             
     DWtitptpa                       60a   Inz(*blanks)                                             
     DWtititoa                       60a   Inz(*blanks)                                             
     DWtitidea                       60a   Inz(*blanks)                                             
     DWtitpdea                       60a   Inz(*blanks)                                             
     DWtitalaa                       60a   Inz(*blanks)                                             
     DWtitcon                        60a   Inz(*blanks)                                             
     DWtitvli                        60a   Inz(*blanks)                                             
     DWtitcan                        60a   Inz(*blanks)                                             
     DWtitvlr                        60a   Inz(*blanks)                                             
     DWtittdc                        60a   Inz(*blanks)                                             
     DWtitnot1                      200a   Inz(*blanks)                                             
     DWtitnot2                      200a   Inz(*blanks)                                             
     DWtitcef                        60a   Inz(*blanks)                                             
     DWtitcam                        60a   Inz(*blanks)                                             
     DWtitpde                        60a   Inz(*blanks)                                             
     DWtitala                        60a   Inz(*blanks)                                             
     DWtitcal                        60a   Inz(*blanks)                                             
     DWbot01                         30a   Inz(*blanks)                                             
     DWbot02                         30a   Inz(*blanks)                                             
     DWbot03                         30a   Inz(*blanks)                                             
     DWbot04                         30a   Inz(*blanks)                                             
     DWbot05                         30a   Inz(*blanks)                                             
     DWbot06                         30a   Inz(*blanks)                                             
     DWbot07                         30a   Inz(*blanks)                                             
     DWcampant                        4a   Inz(*blanks)                                             
     DWdcto                          15a   Inz(*blanks)                                             
     DWcefe                           5  0 Inz(*ZEROS)                                              
     DWcam1                           4a   Inz(*blanks)                                             
     DWcam2                           4a   Inz(*blanks)                                             
     DWcam3                           4a   Inz(*blanks)                                             
     DWpedd1                          9  0 Inz(*ZEROS)                                              
     DWpedd2                          9  0 Inz(*ZEROS)                                              
     DWpedd3                          9  0 Inz(*ZEROS)                                              
     DWalas1                         10a   Inz(*blanks)                                             
     DWalas2                         10a   Inz(*blanks)                                             
     DWalas3                         10a   Inz(*blanks)                                             
     DWstatus                         1a   Inz(*blanks)                                             
     DWdestatus                     120a   Inz(*blanks)                                             
     DDsResultados     Ds                  Occurs(100)                                              
     DGSector                        10a                                                            
     DGCedula                         9  0                                                          
     DGNombre                        30a                                                            
     DGPedidoB                        5  0                                                          
     DGMetPedA                        5  0                                                          
     DGPedTotA                        5  0                                                          
     DGCumPedA                        2a                                                            
     DGPedPagA                        5  0                                                          
     DGInsTotA                        5  0                                                          
     DGInsDelA                        9  0                                                          
     DGPedDelA                        9  0                                                          
     DGAlasA                         10a                                                            
     DGMetPed                         5  0                                                          
     DGPedTot                         5  0                                                          
     DGCumPed                         2a                                                            
     DGInsTot                         5  0                                                          
     DGPedDel                         9  0                                                          
     DGAlas                          10a                                                            
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DDsDetalleR       Ds                  Occurs(5)                                                
     DRConcepto                      50a                                                            
     DRValorInd                      15a                                                            
     DRCantidad                       5  0                                                          
     DRValor                         15a                                                            
                                                                                                    
     DResulSetZon      Ds                  Occurs(999) Qualified                                    
     DSelec                           1a                                                            
     DW_ZONA                          3a                                                            
     D                 ds                                                                           
     Dw_WADTNDÑ1               1      4                                                             
     dw_WADTNDÑ2               5      8                                                             
     dw_WADTNDÑ                1      8                                                             
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DDAyuda                           N   Overlay(Indic:1)                                         
     DSalir                            N   Overlay(Indic:3)                                         
     DGenerar                          N   Overlay(Indic:5)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
                                                                                                    
     DDsNewPlanGu      Ds                                                                           
     DIcaso                           7a   Inz(*Blanks)                                             
     DIcedula                        30a   Inz(*Blanks)                                             
     DIfecha                         10a   Inz(*Blanks)                                             
     DOrespuesta                      1a   Inz(*Blanks)                                             
      **Definición de Procedimientos                                                                
     D InformeR        Pr                                                                           
     DPrSpool          pr                                                                           
     DPrFileNew        pr                                                                           
     DPrCentrar        Pr            50A                                                            
     D pCadena                       50A   Const                                                    
     Dcomando          PR                  ExtPgm('QCMDEXC')                                        
     D                              500A   Options(*VarSize) Const                                  
     D                               15P 5 Const                                                    
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10                                                             
     DPres                            2                                                             
     DKciaR                          10                                                             
     DPgmPais1         Pr                  Extpgm('VOTREP00/VTMGOI8R')                              
     DXcia                            3                                                             
     Dwpais                          30                                                             
                                                                                                    
     DPgmCampaÑa       Pr                  Extpgm('VOTREP00/VTMGOIPR')                              
     DXcia                            3a                                                            
     DXCamp                           4a                                                            
     DXInd                            1A                                                            
                                                                                                    
     Dpgmzona          Pr                  Extpgm('VOTREP00/VTMGOIOR')                              
     DCia                             3                                                             
     DICam                            4                                                             
     DNumZon                          3  0                                                          
     Drespuesta                      30                                                             
     DEstrucZna                            LikeDs(ResulSetZon)                                      
                                                                                                    
     DPrProceso        Pr                                                                           
     DValidaCamps      Pr             1N                                                            
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
     DCAMBIARCq        PR                  Extpgm('VOTREP00/VTMGOIQR')                              
     DWADTNDÑ                        30                                                             
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
      *Converción de cédula a código intero                                                         
     DConversionID     Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DPcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
     DPgmValidarDif    Pr                  Extpgm('VOTREP00/VTPSGC9L')                              
     DPcia                            3                                                             
     DPcam                            4                                                             
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /free                                                                                         
       Dow *In03 = *off;                                                                            
           PTitulo = PrCentrar('Consulta Control de Pagos Guías');                                  
           Clear Blanco;                                                                            
           Write Blanco;                                                                            
           Exfmt principal;                                                                         
           Clear Perror;                                                                            
           If POPPPAIS=1;                                                                           
              clear POPPPAIS;                                                                       
              compania = *Blanks;                                                                   
              PgmPais1(compania:wpais);                                                             
              PDESPAI = wpais;                                                                      
              If PDESPAI = *Blanks;                                                                 
                 *In30=*On;                                                                         
              Else;                                                                                 
                 *In30= *Off;                                                                       
                 *In31=*On;                                                                         
              EndIf;                                                                                
              Clear PDESCAMP;                                                                       
              Clear PDESZONA;                                                                       
              Iter;                                                                                 
           EndIf;                                                                                   
           If PPCAMPAÑA = 1;                                                                        
              Clear PPCAMPAÑA;                                                                      
              Clear PDESCAMP;                                                                       
              Clear PDESZONA;                                                                       
              Callp PgmCampaÑa(compania:PDESCAMP:XInd);                                             
              If PDESCAMP = *Blanks;                                                                
                 *In31=*On;                                                                         
              Else;                                                                                 
                 *In31= *Off;                                                                       
                 *In32=*On;                                                                         
              EndIf;                                                                                
              Iter;                                                                                 
           EndIF;                                                                                   
           If PPZONA = 1;                                                                           
              Clear PPZONA;                                                                         
              Clear PDESZONA;                                                                       
              Clear NumZon;                                                                         
              clear respuesta;                                                                      
              Clear *all ResulSetZon;                                                               
              icam= %trim(PDESCAMP);                                                                
              Callp(e) PgmZona(compania:icam:NumZon:respuesta:ResulSetZon);                         
              If NumZon = 1;                                                                        
                 %Occur(ResulSetZon)= NumZon;                                                       
                 PDESZONA = ResulSetZon.W_ZONA;                                                     
              elseIf %Trim(respuesta) = 'TODAS LAS ZONAS';                                          
                 PDESZONA = respuesta;                                                              
              elseIf NumZon > 1;                                                                    
                 PDESZONA = 'VARIAS ZONAS';                                                         
              EndIf;                                                                                
              If PDESZONA = *Blanks;                                                                
                 *In32=*On;                                                                         
              Else;                                                                                 
                 *In32= *Off;                                                                       
                 *In30=*On;                                                                         
              EndIf;                                                                                
              Iter;                                                                                 
           EndIF;                                                                                   
           If *In03=*on;                                                                            
              Leave;                                                                                
           EndIf;                                                                                   
           If *In12  =*on;                                                                          
              Clear principal;                                                                      
              *In12 =*off;                                                                          
              iter;                                                                                 
           EndIf;                                                                                   
           If *in05 =*on;                                                                           
             PrProceso();                                                                           
             PgmValidarDif(Compania:PCamR);                                                         
              If ValidaCamps = *Off;                                                                
                    PrValidarPagosC();                                                              
                    PrValidarPagos();                                                               
                    PrValidarPagosT();                                                              
                    InformeR();                                                                     
                    PMSJ1='    La Consulta se ha generado Exitosamente]]';                          
                    Pfile = 'VTWLPGF';                                                              
                    Plib = User;                                                                    
                    Write Blanco;                                                                   
                    exfmt mensaje;                                                                  
                    clear principal;                                                                
                    PrSpool();                                                                      
                    Iter;                                                                           
              EndIf;                                                                                
              *in05  =*off;                                                                         
           EndIf;                                                                                   
         If InfDsp.key = X'F1';                                                                     
           If ValidaCamps = *Off;                                                                   
              Iter;                                                                                 
           EndIf;                                                                                   
           EndIf;                                                                                   
       EndDo;                                                                                       
       PrFileNewxx();                                                                               
        *inlr=*on;                                                                                  
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////////                    
      //Prototipo: ValidaCamps                                                //                    
      //           Valida Campos para la consulta Obligatorios                //                    
      //////////////////////////////////////////////////////////////////////////                    
     PValidaCamps      B                                                                            
     DValidaCamps      Pi             1N                                                            
     DIndCam           S              1N   Inz(*Off)                                                
      /Free                                                                                         
        *in30=*Off;                                                                                 
        *in31=*Off;                                                                                 
        *in32=*Off;                                                                                 
        Clear Perror;                                                                               
        If PDESPAI = *Blanks;                                                                       
           *in30=*On;                                                                               
           Perror = 'Ingrese la compa¦ia';                                                          
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
        EndIf;                                                                                      
        If PDESCAMP=*Blanks;                                                                        
           *In31 = *On;                                                                             
           Perror = 'Seleccione Campa¦a';                                                           
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
        EndIf;                                                                                      
       Return IndCam;                                                                               
      /End-Free                                                                                     
     PValidaCamps      E                                                                            
                                                                                                    
     P*--------------------------------------------------                                           
     P* Procedure name: Informe                                                                     
     P* Purpose:        realizar el informe de los cupos reversados                                 
     P* Returns:                                                                                    
     P*--------------------------------------------------                                           
     P InformeR        B                                                                            
     D InformeR        PI                                                                           
                                                                                                    
      /Free                                                                                         
         open VTPSGC0I;                                                                             
         PFECHA=%char(%date());                                                                     
         PPGM=nomp;                                                                                 
         PCAMPA = PDESCAMP;                                                                         
         Write Titulos;                                                                             
                                                                                                    
         PCamE= PDESCAMP;                                                                           
         PCamR=*Blanks;                                                                             
         POper='-';                                                                                 
         Pnume='001';                                                                               
         proCampa(Compania:PCamE:PCamR:POper:Pnume);                                                
         If PCamR = '2009';                                                                         
          PCamR = '2008';                                                                           
         Endif;                                                                                     
                                                                                                    
         SqlStr = *Blanks;                                                                          
         SqlStr = 'Select * '+                                                                      
                  'From Votrea00/VTWLPGF ' +                                                        
                  'Where LPGCIA = ' + '''' + Compania  + '''' + ' And ' +                           
                  'LPGCMP = ' + '''' + %TRIM(PCamR)   + '''' ;                                      
                                                                                                    
         If PDESZONA <> *Blanks;                                                                    
          SqlStr = %trim(SqlStr) + ' And LPGZNA In (';                                              
          for dx2 = 1 to NumZon;                                                                    
              %OCCUR(ResulSetZon) = dx2;                                                            
              SqlStr = %trim(SqlStr) + $C + ResulSetZon.W_ZONA + $C;                                
              If dx2 < NumZon ;                                                                     
              SqlStr = %trim(SqlStr) + ' , ';                                                       
              EndIf;                                                                                
          EndFor;                                                                                   
          SqlStr = %trim(SqlStr) + ' )';                                                            
         EndIf;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur3 From :SqlStr;                                                                
                                                                                                    
         Exec Sql                                                                                   
          Declare Cursor3 Cursor For Cur3;                                                          
                                                                                                    
        Exec SQL                                                                                    
          open cursor3;                                                                             
                                                                                                    
        Exec SQL                                                                                    
          fetch cursor3 Into :Fields;                                                               
                                                                                                    
        Dow sqlcod=*zeros;                                                                          
            PLPGCIA = QLPGCIA ;                                                                     
            PLPGCMP = QLPGCMP ;                                                                     
            PLPGREG = QLPGREG ;                                                                     
            PLPGZNA = QLPGZNA ;                                                                     
            PLPGSEC = QLPGSEC ;                                                                     
            PLPGCDG = QLPGCDG ;                                                                     
            PLPGNOM = QLPGNOM ;                                                                     
            PLPGTDS = QLPGTDS ;                                                                     
            PLPGVDE = QLPGVDE ;                                                                     
            PLPGVDC = QLPGVDC ;                                                                     
            PLPGFDE = QLPGFDE ;                                                                     
            PLPGFDC = QLPGFDC ;                                                                     
            PLPGVRC = QLPGVRC ;                                                                     
            IF QLPGDSA = 'SI';                                                                      
             PLPGVRC = 0;                                                                           
            ENDIF;                                                                                  
            PLPGFVR = QLPGFVR ;                                                                     
            PLPGDSA = QLPGDSA ;                                                                     
            PLPGMOT = QLPGMOT ;                                                                     
            PLPGIPT = QLPGIPT ;                                                                     
            If *In99=*on;                                                                           
               PCAMPA = PDESCAMP;                                                                   
               PPGM=nomp;                                                                           
               Write Titulos;                                                                       
               *In99=*off;                                                                          
            EndIf;                                                                                  
            Write Detalle;                                                                          
            Exec SQL                                                                                
              fetch cursor3 Into :Fields;                                                           
        EndDo;                                                                                      
        Write PPIEPAG;                                                                              
        PrFileNew();                                                                                
                                                                                                    
        Exec SQL                                                                                    
          Close cursor3;                                                                            
                                                                                                    
       close VTPSGC0I;                                                                              
      /END-FREE                                                                                     
     P InformeR        E                                                                            
                                                                                                    
     P*--------------------------------------------------                                           
     P* Procedure Name: PrSpool                                                                     
     P* Purpose       : Ir al Spool                                                                 
     P*--------------------------------------------------                                           
     PPrSpool          B                                                                            
     DPrSpool          PI                                                                           
      /Free                                                                                         
       Clear Cmd;                                                                                   
       Clear Long;                                                                                  
       Cmd = 'WRKSPLF';                                                                             
       Long = %Len(%Trim(Cmd));                                                                     
       Comando(Cmd:Long);                                                                           
      /End-Free                                                                                     
     PPrSpool          E                                                                            
                                                                                                    
     P*---------------------------------------------------------                                    
     P* Procedure Name: PrFileNew                                                                   
     P* Purpose       : Graba Información en Archivo de Trabajo                                     
     P*---------------------------------------------------------                                    
     PPrFileNew        B                                                                            
     DPrFileNew        PI                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
                                                                                                    
       SqlStr = 'Drop Table ' + %Trim(User) + '/VTWLPGF';                                           
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
       SqlStr = 'Delete From ' + %Trim(User) + '/VTWLPGF';                                          
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
       PCamE= PDESCAMP;                                                                             
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       Pnume='001';                                                                                 
       proCampa(Compania:PCamE:PCamR:POper:Pnume);                                                  
       If PCamR = '2009';                                                                           
        PCamR = '2008';                                                                             
       Endif;                                                                                       
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Create Table ' + %Trim(User) + '/VTWLPGF As (' +                                   
                'Select * '+                                                                        
                'From Votrea00/VTWLPGF ' +                                                          
                'Where LPGCIA = ' + '''' +%trim(Compania)  + '''' + ' And ' +                       
                'LPGCMP = ' + '''' + %TRIM(PCamR)   + '''' ;                                        
                                                                                                    
       If PDESZONA <> *Blanks;                                                                      
          SqlStr = %trim(SqlStr) + ' And LPGZNA In (';                                              
          for dx1 = 1 to NumZon;                                                                    
              %OCCUR(ResulSetZon) = dx1;                                                            
              SqlStr = %trim(SqlStr) + $C + ResulSetZon.W_ZONA + $C;                                
              If dx1 < NumZon ;                                                                     
                 SqlStr = ' '+%trim(SqlStr) + ' , ';                                                
              EndIf;                                                                                
          EndFor;                                                                                   
          SqlStr = ' '+%trim(SqlStr) + ' )';                                                        
       EndIf;                                                                                       
       SqlStr = %trim(SqlStr) + ' ) With Data';                                                     
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
       Clear  SqlStr;                                                                               
       SqlStr = ' ALTER TABLE ' + %Trim(User) + '/VTWLPGF'+                                         
                ' ADD COLUMN LPGMPD CHARACTER(5)  NOT HIDDEN'+                                      
                ' ADD COLUMN LPGPDT CHARACTER(5)  NOT HIDDEN'+                                      
                ' ADD COLUMN LPGCMT CHARACTER(2)  NOT HIDDEN'+                                      
                ' ADD COLUMN LPGPTP CHARACTER(5)  NOT HIDDEN'+                                      
                ' ADD COLUMN LPGCBS CHARACTER(5)  NOT HIDDEN'+                                      
                ' ADD COLUMN LPGPPC NUMERIC(6, 0) NOT HIDDEN'+                                      
                ' ADD COLUMN LPGPPV CHARACTER(13) NOT HIDDEN'+                                      
                ' ADD COLUMN LPGPPA NUMERIC(6, 0) NOT HIDDEN'+                                      
                ' ADD COLUMN LPGPAV CHARACTER(13) NOT HIDDEN'+                                      
                ' ADD COLUMN LPGINC CHARACTER(5)  NOT HIDDEN'+                                      
                ' ADD COLUMN LPGINR CHARACTER(5)  NOT HIDDEN'+                                      
                ' ADD COLUMN LPGIRV CHARACTER(13) NOT HIDDEN'+                                      
                ' ADD COLUMN LPGVRT NUMERIC(15, 2) NOT HIDDEN' +                                    
                ' ADD COLUMN LPGNAL CHARACTER(20) NOT HIDDEN';                                      
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
       Clear  SqlStr;                                                                               
                //* Estableciendo las etiquetas de columna para USER.VTWLPGF */                     
       SqlStr = 'LABEL ON COLUMN '+%Trim(User)+'/VTWLPGF ( '+                                       
                ' LPGCBS TEXT IS '+ $C +'Cupo Base'+$C+' ,'+                                        
                ' LPGMPD TEXT IS '+ $C +'Meta Pedido'+$C+' ,'+                                      
                ' LPGPDT TEXT IS '+ $C +'Pedidos Totales'+$C +' ,'+                                 
                ' LPGCMT TEXT IS '+ $C +'Cumplió Meta'+$C+' ,'+                                     
                ' LPGPTP TEXT IS '+ $C +'Pedidos Totales Pagados'+ $C +' ,'+                        
                ' LPGPPC TEXT IS '+$C+'Pedidos Totales pagados Cantidad'+$C+','+                    
                ' LPGPPV TEXT IS '+ $C +'Pedidos Totales pagados Valor'+ $C+','+                    
                ' LPGPPA TEXT IS '+ $C +'Ped pag adic cant'+ $C +' ,'+                              
                ' LPGPAV TEXT IS '+ $C +'Ped pag adic vlr'+ $C +' ,'+                               
                ' LPGINC TEXT IS '+ $C +'Inscripciones Cantidad'+ $C +' ,'+                         
                ' LPGINR TEXT IS '+ $C+'Inscripciones Rentable Cantidad'+$C+','+                    
                ' LPGIRV TEXT IS '+ $C +'Inscrip Rent Valor'+ $C + ','+                             
                ' LPGVRT TEXT IS '+ $C +'Valor Retenido'+ $C +',' +                                 
                ' LPGNAL TEXT IS '+ $C +'Nivel de Alas'+ $C +'  )';                                 
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
       Clear  SqlStr;                                                                               
               //* Estableciendo las cabeceras de columna para USER.VTWLPGF */                      
       SqlStr ='LABEL ON COLUMN '+ %Trim(User)+'/VTWLPGF ('+                                        
               ' LPGCBS IS '+ $C +'Cupo Base '+ $C +' ,'+                                           
               ' LPGMPD IS '+ $C +'Meta Pedido'+ $C +' ,'+                                          
               ' LPGPDT IS '+ $C +'Pedidos Totales'+ $C +' ,'+                                      
               ' LPGCMT IS '+ $C +'Cumplió Meta'+ $C +' ,'+                                         
               ' LPGPTP IS '+ $C +'Pedidos Totales'+ $C +' ,'+                                      
               ' LPGPPC IS '+ $C +'Pedidos totales'+ $C +' ,'+                                      
               ' LPGPPV IS '+ $C +'Pedidos totales p va'+ $C +' ,'+                                 
               ' LPGPPA IS '+ $C +'Ped pag adic cant   '+ $C +' ,'+                                 
               ' LPGPAV IS '+ $C +'Ped pag adic vlr    '+ $C +' ,'+                                 
               ' LPGINC IS '+ $C +'Inscripciones Cant  '+ $C +' ,'+                                 
               ' LPGINR IS '+ $C +'Inscripciones Rent  '+ $C +' ,'+                                 
               ' LPGIRV IS '+ $C +'Inscrip Rent Vlr    '+ $C +' ,'+                                 
               ' LPGVRT IS '+ $C +'Valor Retenido      '+ $C +' ,'+                                 
               ' LPGNAL IS '+ $C +'Nivel de Alas'+ $C +'  )';                                       
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
       Pr_CargarIformacionAlter();                                                                  
      /End-Free                                                                                     
     PPrFileNew        E                                                                            
                                                                                                    
     P*---------------------------------------------------------                                    
     P* Procedure Name: Pr_CargarIformacionAlter                                                    
     P* Purpose       : Recorre archivo pago lideres VtwLpgf                                        
     P*---------------------------------------------------------                                    
     PPr_CargarIformacionAlter...                                                                   
     P                 B                                                                            
     DPr_CargarIformacionAlter...                                                                   
     D                 PI                                                                           
     Dw_Zona           s              3    Inz(*Blanks)                                             
     Dw_VlrRetenido    s             15  0 Inz(*Zeros)                                              
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select distinct LPGZNA From ' + %Trim(User) + '/VTWLPGF '+                         
                '';                                                                                 
           Exec Sql Prepare C0 From :SqlStr;                                                        
           Exec Sql Declare C0 Cursor For C0;                                                       
           Exec Sql Open C0;                                                                        
           Exec Sql fetch C0 Into  :w_Zona;                                                         
           Dow SqlCod = *Zeros;                                                                     
               Clear DsResultadosGu;                                                                
               Clear DsDetalleGuias;                                                                
               Clear *all DsResultados;                                                             
               SPcia = Compania;                                                                    
               Pres = *Blanks;                                                                      
               PciaR = *Blanks;                                                                     
               CompaÑia(SPcia:Pres:PciaR);                                                          
               WCIA  =   PciaR;                                                                     
               WZONA =   w_Zona;                                                                    
               WCAMP =   PDESCAMP;                                                                  
                                                                                                    
               If Pres= 'Si';                                                                       
                  Exec SQL                                                                          
                   Call VOTREP00.GSL_RESULTADOSGUIAS(                                               
                   :Wcia,:Wzona,:Wcamp,:Wnrosec,:Wfoco,:Wtitpanta0,:Wtitpanta1,                     
                   :Wtitpanta2,:Wtitpanta3,:Wtitsec,:Wtitced,:Wtitnom,:Wtitped,                     
                   :Wtitmpta,:Wtitptoa,:Wtitcpea,:Wtitptpa,:Wtititoa,:Wtitidea,                     
                   :Wtitpdea,:Wtitalaa,:Wtitmpe,:Wtitpto,:Wtitcpe,:Wtitito,                         
                   :Wtitpde, :Wtitala,:Wstatus,:Wdestatus);                                         
                                                                                                    
                  Exec SQL                                                                          
                   Associate Result Set Locators                                                    
                   (:RsTramaResult) With Procedure Votrep00.Gsl_ResultadosGuias;                    
                                                                                                    
                  Exec SQL                                                                          
                       Allocate Resultados Cursor                                                   
                       For Result Set :RsTramaResult;                                               
                                                                                                    
                  Exec SQL                                                                          
                       Fetch Resultados For 100 Rows Into :DsResultados;                            
                                                                                                    
                  Exec SQL                                                                          
                       Close Resultados;                                                            
                  SqlStr = *Blanks;                                                                 
                  SqlStr = 'Select lpgcdg From ' +                                                  
                           %Trim(User) + '/VTWLPGF '+                                               
                           ' where Lpgzna = ' + $C + w_Zona         + $C +                          
                           '   And Lpgcia = ' + $C +%trim(Compania) + $C +                          
                           '   And Lpgcmp = ' + $C +%trim(PCamR)    + $C ;                          
                                                                                                    
                  Exec Sql Prepare C1 From :SqlStr;                                                 
                  Exec Sql Declare C1 Cursor For C1;                                                
                  Exec Sql Open C1;                                                                 
                  Exec Sql fetch C1 Into  :Qlpgcdg;                                                 
                  Clear IDX;                                                                        
                  Dow SqlCod = *Zeros;                                                              
                      For idx = 1 to %Elem(DsResultados);                                           
                       %Occur(DsResultados) = Idx;                                                  
                       If GCedula = Qlpgcdg;                                                        
                          Isector  = GSector;                                                       
                          Icedulag = GCedula;                                                       
                          Inombreg = GNombre;                                                       
                          Ifoco    = Wfoco;                                                         
                          Ipedbas  = GPedidoB;                                                      
                          Imetpeda = GMetPedA;                                                      
                          Ipedtota = GPedTotA;                                                      
                          Icummet  = GCumPedA;                                                      
                          Ipedpaga = GPedPagA;                                                      
                          Iinstota = GInsTotA;                                                      
                          Iinsdela = GInsDelA;                                                      
                          Ipeddela = GPedDelA;                                                      
                          Ialasa   = GAlasA;                                                        
                          Pr_DetallePagosCompradora();                                              
                          //* Recupera Valor retenido                                               
                          Clear w_VlrRetenido;                                                      
                          Exec Sql                                                                  
                           SELECT Sum(IfNull(VRFVRR , 0)) Into :w_VlrRetenido                       
                           FROM  Vtwvrff                                                            
                           Where Vrfcia =:Compania                                                  
                             And Vrfced =:Qlpgcdg                                                   
                             And Vrfzna =:w_Zona                                                    
                             And Vrfcam =:PCamR ;                                                   
                       Clear w_ZNA;                                                                 
                       w_ZNA = w_Zona;                                                              
                       PrRecuperaPosteo();//Recupera Valor posteado Cartera                         
                       PrRecupPostePend();//Recupera Valor Pago lider                               
                       Clear ZLpgtds;                                                               
                       Exec Sql                                                                     
                        Select Sum(Spgvds) Into :ZLpgtds                                            
                        From Votrea00/Vtmspgf                                                       
                        Where Spgcia =:Compania And                                                 
                              Spgced =:Qlpgcdg And                                                  
                      //      Spgzon =:w_Zona And                                                   
                      //      Spgsec =:GSector And                                                  
                              Spgcam =:PCamR;                                                       
                                                                                                    
                       Clear ZLpgvdc;                                                               
                       Exec Sql                                                                     
                        Select Sum(Spgvds) Into :ZLpgvdc                                            
                        From Votrea00/Vtmspgf                                                       
                        Where Spgcia =:Compania And                                                 
                              Spgced =:Qlpgcdg And                                                  
                      //      Spgzon =:w_Zona And                                                   
                              Spgcam =:PCamR And                                                    
                              Spgccr = 042;                                                         
                                                                                                    
                       Clear IndPro;                                                                
                       Exec Sql                                                                     
                        Select 'S' Into :IndPro                                                     
                        From Votrea00/Vtmspgf                                                       
                        Where Spgcia =:Compania And                                                 
                              Spgced =:Qlpgcdg And                                                  
                              Spgzon =:w_Zona And                                                   
                              Spgcam =:PCamR And                                                    
                              Spginp = 'E';                                                         
                       Clear  SqlStr;                                                               
                       SqlStr = ' Update ' + %Trim(User) + '/VTWLPGF Set '+                         
                     ' Lpgtds = ' + $C + %Trim(%Editc(ZLpgtds:'P')) + $C + ','+                     
                     ' Lpgmpd = ' + $C + %Trim(%Editc(Imetpeda:'Z')) + $C + ','+                    
                     ' Lpgpdt = ' + $C + %Trim(%Editc(Ipedtota:'Z')) + $C + ','+                    
                     ' Lpgcmt = ' + $C + %Trim(Icummet )             + $C + ','+                    
                     ' Lpgptp = ' + $C + %Trim(%Editc(Ipedpaga:'Z')) + $C + ','+                    
                     ' Lpgcbs = ' + $C + %Trim(%Editc(Ipedbas:'Z'))  + $C + ',';                    
                   //Verifica en archivo de Bancos que el pago se haya realizado                    
                   Clear Tced;                                                                      
                   Clear Tcns;                                                                      
                   Clear W_IndPagoBanco;                                                            
                   Tcns = %Trim(%Char(Qlpgcdg));                                                    
                   ConversionID(Compania:Tced:Tcns);                                                
                   Tced15 = %Int(%Trim(Tced));                                                      
                   Exec sql                                                                         
                   SELECT PGEEST Into :W_IndPagoBanco                                               
                   FROM votrea00.vtwpgef                                                            
                   WHERE PGECDG = Trim(:Tced15)                                                     
                     And PGECIA = Trim(:Compania)                                                   
                     And PGECMP = Trim(:PCamR);                                                     
                                                                                                    
                If W_IndPagoBanco ='I';                                                             
                   SqlStr = %Trim(SqlStr ) + ' Lpgvde = 0,';                                        
                ElseIf w_AcuEfe > *Zeros;                                                           
                   SqlStr = %Trim(SqlStr ) +                                                        
                     ' LPGVDE = ' + $C + %Trim(%Editc(w_AcuEfe:'P')) + $C + ',';                    
                EndIf;                                                                              
                If w_AcuCar > *Zeros;                                                               
                   Clear w_AcuCarP;                                                                 
                   w_AcuCarP = w_AcuCar;                                                            
                   If IndPro = 'S';                                                                 
                    w_AcuCarP = 0;                                                                  
                   Endif;                                                                           
                   SqlStr = %Trim(SqlStr ) +                                                        
                     ' LPGVDC = ' + $C + %Trim(%Editc(ZLpgvdc:'P')) + $C +  ','+                    
              //     ' LPGVDC = ' + $C + %Trim(%Editc(w_AcuCar:'P')) + $C + ','+                    
                     ' LPGVRC = ' + $C + %Trim(%Editc(w_AcuCarP:'P')) + $C +',';                    
                EndIf;                                                                              
                %Occur(DsDetalleR)   = 1;                                                           
                SqlStr = %Trim(SqlStr ) +                                                           
                ' Lpgppc = ' + %Trim(%Editc(RCantidad:'P')) + ','           +                       
                     ' Lpgppv = ' + $C + %Trim(RValor) + $C + ','           ;                       
                %Occur(DsDetalleR)   = 2;                                                           
                SqlStr = %Trim(SqlStr ) +                                                           
                ' Lpgppa = ' + %Trim(%Editc(RCantidad:'P')) + ','           +                       
                     ' Lpgpav = ' + $C + %Trim(RValor) + $C + ','           +                       
                     ' Lpginc = ' + $C + %Trim(%Editc(Iinstota:'P')) + $C + ','+                    
                     ' Lpginr = ' + $C + %Trim(%Editc(Iinsdela:'P')) + $C + ',';                    
                %Occur(DsDetalleR)   = 3;                                                           
                Cnades = *Blanks;                                                                   
                Exec Sql                                                                            
                 Select Cnades Into :Cnades                                                         
                 From Votrea00/vtmcnaf                                                              
                 Where Cnacia = Trim(:Compania) And                                                 
                       Cnacmp = Trim(:PCamR) And                                                    
                       Cnasec = Trim(:Isector) And                                                  
                       Cnacdg = Trim(Char(:Qlpgcdg))                                                
                 Fetch First 1 Row Only;                                                            
                                                                                                    
                If Cnades = ' ';                                                                    
                 Cnades = '2 Alas';                                                                 
                Endif;                                                                              
                SqlStr = %Trim(SqlStr ) +                                                           
                     ' Lpgirv = ' + $C + %Trim(RValor) + $C +','+                                   
                     ' LpgVrt = ' + %Trim(%Editc(w_VlrRetenido:'P')) + ','+                         
                     ' LpgNal = ' + $C + %Trim(Cnades) + $C +                                       
                           ' where Lpgzna = ' + $C + w_Zona         + $C +                          
                           '   And Lpgcia = ' + $C +%trim(Compania) + $C +                          
                           '   And Lpgcdg = ' + %Char(Qlpgcdg)  +                                   
                           '   And Lpgsec = ' + $C +%trim(Isector) + $C +                           
                           '   And Lpgcmp = ' + $C +%trim(PCamR)    + $C ;                          
                                                                                                    
                        Exec Sql                                                                    
                         Execute Immediate :SqlStr;                                                 
                                                                                                    
                       EndIf;                                                                       
                      EndFor;                                                                       
                      Exec Sql fetch C1 Into  :Qlpgcdg;                                             
                  EndDo;                                                                            
                  Exec Sql close C1;                                                                
                                                                                                    
               EndIf;                                                                               
               Exec Sql fetch C0 Into  :w_Zona;                                                     
           EndDo;                                                                                   
           Exec Sql Close C0;                                                                       
      /End-Free                                                                                     
     PPr_CargarIformacionAlter...                                                                   
     P                 E                                                                            
                                                                                                    
     P*---------------------------------------------------------                                    
     P* Procedure Name: Pr_DetallePagosCompradora...                                                
     P* Purpose       : Recorre archivo pago lideres VtwLpgf                                        
     P*---------------------------------------------------------                                    
     PPr_DetallePagosCompradora...                                                                  
     P                 B                                                                            
     DPr_DetallePagosCompradora...                                                                  
     D                 PI                                                                           
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       Clear *all DsDetalleR;                                                                       
                                                                                                    
       Exec SQL                                                                                     
        CALL VOTREP00.GSL_DETALLEDCTOGUIAS(:Wcia,                                                   
        :Wzona,:Wcamp,:Isector,:Icedulag,:Inombreg,:Ifoco,:Ipedbas,                                 
        :Imetpeda,:Ipedtota,:Icummet,:Ipedpaga,:Iinstota,:Iinsdela,                                 
        :Ipeddela,:Ialasa,:Wtitpanta0,:Wtitsec,:Wtitfoc,:Wtitced,                                   
        :Wtitnom,:Wtitped,:Wtitmpea,:Wtitptoa,:Wtitcpea,:Wtitptpa,                                  
        :Wtititoa,:Wtitidea,:Wtitpdea,:Wtitalaa,:Wtitcon,:Wtitvli,                                  
        :Wtitcan,:Wtitvlr,:Wtittdc,:Wtitnot1,:Wtitnot2,:Wtitcef,                                    
        :Wtitcam,:Wtitpde,:Wtitala,:Wtitcal,:Wbot01,:Wbot02,:Wbot03,                                
        :Wbot04,:Wbot05,:Wbot06,:Wbot07,:Wcampant,:Wdcto,:Wcefe,:Wcam1,:Wcam2,                      
        :Wcam3,:Wpedd1,:Wpedd2,:Wpedd3,:Walas1,:Walas2,:Walas3,                                     
        :Wstatus,:Wdestatus,:PtitRtf1,:PtitRtf2,:PtitRtf3,:PtitRtf4,:PtitRtf5,                      
        :PtitRtf6,:PVlrRtf1,:PVlrRtf2,:PVlrRtf3,:PVlrRtf4,:PVlrRtf5,:PVlrRtf6);                     
                                                                                                    
       Exec SQL                                                                                     
        Associate Result Set Locators                                                               
        (:RsTramaDetalle) With Procedure VOTREP00.GSL_DETALLEDCTOGUIAS;                             
                                                                                                    
       Exec SQL                                                                                     
              Allocate Resultados1 Cursor                                                           
              For Result Set :RsTramaDetalle;                                                       
                                                                                                    
       Exec SQL                                                                                     
              Fetch Resultados1 For 5 Rows Into :DsDetalleR;                                        
                                                                                                    
       Exec SQL                                                                                     
              Close Resultados1;                                                                    
      /End-Free                                                                                     
     PPr_DetallePagosCompradora...                                                                  
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Graba Información en el Archivo Vtwlpgf         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrRecuperaPosteo...                                                                           
     P                 B                                                                            
     DPrRecuperaPosteo...                                                                           
     D                 Pi                                                                           
     Dw_campana        s              4A                                                            
     DW_CmpÑ           s              4A                                                            
      /Free                                                                                         
       //Verifica si Zona es parte de nuevo plan de Guías                                           
       w_campana=PCamR;                                                                             
       Clear DsNewPlanGu;                                                                           
       Icedula = %trim(%editc(Qlpgcdg:'P'));                                                        
       PgmNewPlanG(Compania:w_ZNA:Icaso:Icedula:Ifecha:Orespuesta);                                 
       Clear w_AcuCar;                                                                              
       Clear w_AcuEfe;                                                                              
       Cmd = *Blanks;                                                                               
       If Compania = '001';                                                                         
        Cmd = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + %trim(Compania) +                                  
              '/VLARDTL01) ' +                                                                      
              'OVRSCOPE(*JOB)';                                                                     
       Else;                                                                                        
        Cmd = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + %trim(Compania) + '/PARDTL)'                       
              + 'OVRSCOPE(*JOB)';                                                                   
       Endif;                                                                                       
       Long = %Len(%Trim(Cmd));                                                                     
       CallP(E) comando(Cmd:Long);                                                                  
            // 'from Opf'+%trim(Compania) +'/pardtl ' +                                             
       SqlStr= 'Select * '   +                                                                      
               'from pardtl ' +                                                                     
               'Where ADCSTÑ = '+ %Char(Qlpgcdg);                                                   
       Exec Sql                                                                                     
        Prepare csr3 From :SqlStr;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare csr3 Cursor For csr3;                                                               
       Exec Sql                                                                                     
        Open csr3;                                                                                  
       Exec Sql                                                                                     
        fetch csr3 Into :Fields1;                                                                   
       Dow SqlCod = *Zeros;                                                                         
           Clear WADTNDÑ;                                                                           
           WADTNDÑ  = qADTNDÑ;                                                                      
           CAMBIARCq(WADTNDÑ);                                                                      
           Clear w_WADTNDÑ;                                                                         
           w_WADTNDÑ = %Trim(WADTNDÑ);                                                              
           Clear W_CmpÑ;                                                                            
           If Orespuesta = 'S';                                                                     
              W_CmpÑ = w_WADTNDÑ2;                                                                  
           Else;                                                                                    
              W_CmpÑ = w_WADTNDÑ1;                                                                  
           Endif;                                                                                   
           If W_CMPÑ = w_campana;                                                                   
              If QADTRAM < *Zeros;                                                                  
                 QADTRAM = QADTRAM * -1;                                                            
                 If QADRDPT = 42 And %Scan('COMPRAYA':QAdtndÑ) <= 0;                                
                    w_AcuCar  += QADTRAM;                                                           
                 ElseIf QADRDPT = 43;                                                               
                    w_AcuEfe  += QADTRAM;                                                           
                 EndIf;                                                                             
              EndIf;                                                                                
           EndIf;                                                                                   
          Exec Sql                                                                                  
            fetch csr3 Into :Fields1;                                                               
       EndDO;                                                                                       
          Exec Sql                                                                                  
            Close csr3;                                                                             
       Clear Cmd;                                                                                   
       Cmd = 'DLTOVR FILE(PARDTL) LVL(*JOB)';                                                       
       Long = %Len(%Trim(Cmd));                                                                     
       CallP(E) comando(Cmd:Long);                                                                  
      /End-Free                                                                                     
     PPrRecuperaPosteo...                                                                           
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Recupera Informacion de Posteo para               //                          
      //////////////////////////////////////////////////////////////////////                        
     PPrRecupPostePend...                                                                           
     P                 B                                                                            
     DPrRecupPostePend...                                                                           
     D                 Pi                                                                           
     D Ds_DatOrd       Ds                   Dim(2) qualified                                        
     D sc_VlrPsto                    15  2                                                          
     D sc_codCCC                      3  0                                                          
      /Free                                                                                         
       Clear Ds_DatOrd;                                                                             
       SqlStr='Select SPGVDS, spgccr '+                                                             
            ' from votrea00.vtmspgf  '+                                                             
            ' where spgccr in (42 , 43)'+                                                           
            ' And spgced = '+ %Editc(Qlpgcdg:'Z')+                                                  
            ' And spgcia='+$c+Compania+$c+                                                          
            ' and spgzon='+$c+wZona +$c+                                                            
            ' and spgsec='+$c+%Trim(Isector)+$c+                                                    
            ' And spgcam='+$c+PCamR+$c+                                                             
            ' Order by spgccr asc';                                                                 
       Exec Sql Prepare pSql From :SqlStr;                                                          
       Exec Sql Declare dcs Cursor For pSql;                                                        
       Exec Sql Open dcs;                                                                           
       Exec Sql Fetch From dcs                                                                      
       For 2         Rows Into :Ds_DatOrd;                                                          
       Exec Sql Close dcs;                                                                          
       If w_AcuEfe = *Zeros;                                                                        
          If Ds_DatOrd(1).sc_codCCC=43;                                                             
             w_AcuEfe = Ds_DatOrd(1).sc_VlrPsto;                                                    
          ElseIf Ds_DatOrd(2).sc_codCCC =43;                                                        
             w_AcuEfe = Ds_DatOrd(2).sc_VlrPsto;                                                    
          EndIf;                                                                                    
       EndIf;                                                                                       
       If w_AcuCar = *Zeros;                                                                        
          If Ds_DatOrd(1).sc_codCCC=42;                                                             
             w_AcuCar = Ds_DatOrd(1).sc_VlrPsto;                                                    
          ElseIf Ds_DatOrd(2).sc_codCCC=42;                                                         
             w_AcuCar = Ds_DatOrd(2).sc_VlrPsto;                                                    
          EndIf;                                                                                    
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrRecupPostePend...                                                                           
     P                 E                                                                            
                                                                                                    
      *------------------------------------------------------------------------                     
      * Centrar cadena alfanumérica                                                                 
      *------------------------------------------------------------------------                     
     PPrCentrar        B                                                                            
     D                 Pi            50A                                                            
     D pCadena                       50A   Const                                                    
                                                                                                    
      *- Variables                                                                                  
     DW_Blanco         S              5S 0                                                          
     DW_Inicio         S              5S 0 Inz(*Zeros)                                              
     DW_Longit         S              5S 0 Inz(*Zeros)                                              
     DW_TxtCen         S             50A   Inz(*Blanks)                                             
      /Free                                                                                         
       //Validar mensaje                                                                            
         If (pCadena = *Blanks) Or                                                                  
            (%Len(%Trim(pCadena)) = 50);                                                            
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
                                                                                                    
      *------------------------------------------------------------------------                     
      * Verifica Pagos Realizados                                                                   
      *------------------------------------------------------------------------                     
     PPrProceso        B                                                                            
     DPrProceso        Pi                                                                           
     DNro              s              5  0 Inz(*Zeros)                                              
     DCia              s              3a   Inz(*BLanks)                                             
     DZon              s              3a   Inz(*Blanks)                                             
     DCed              s              9  0 Inz(*Zeros)                                              
     DCam              s              4a   Inz(*Blanks)                                             
     DCcr              s              3  0 Inz(*Zeros)                                              
      /Free                                                                                         
       PCamE= ICam;                                                                                 
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       If Icam = '2010';                                                                            
        Pnume='002';                                                                                
       Else;                                                                                        
        Pnume='001';                                                                                
       Endif;                                                                                       
       ProCampa(Compania:PCamE:PCamR:POper:Pnume);                                                  
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select * '+                                                                        
                'From Votrea00/VTWLPGF ' +                                                          
                'Where LPGCIA = ' + '''' + Compania  + '''' + ' And ' +                             
                'LPGCMP = ' + '''' + %TRIM(PCamR) + '''';                                           
            //  'And LPGDSA = ' + '''' + 'NO' + '''';                                               
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3I From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor3I Cursor For Cur3I;                                                          
                                                                                                    
       Exec SQL                                                                                     
        Open cursor3I;                                                                              
                                                                                                    
       Exec SQL                                                                                     
        Fetch Cursor3I Into :Fields;                                                                
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= 0;                                                           
        Cia = qLpgcia;                                                                              
        Zon = qLpgzna;                                                                              
        Ced = qLpgcdg;                                                                              
        Cam = qLpgcmp;                                                                              
        If qLpgvde > 0;                                                                             
         Clear Nro;                                                                                 
         Exec Sql                                                                                   
          Select Count(0) Into :Nro                                                                 
          From Votrea00/Vtmspgf                                                                     
          Where Spgcia = :Cia And                                                                   
                Spgzon = :Zon And                                                                   
                Spgced = :Ced And                                                                   
                Spgcam = :Cam And                                                                   
                Spgccr = 043 And                                                                    
                Spgvps > 0 And                                                                      
                (Spginp = ' ' Or Spginp = 'T');                                                     
         If Nro > 0;                                                                                
          Clear Tced;                                                                               
          Clear Tcns;                                                                               
          Tcns = %Trim(%Char(Qlpgcdg));                                                             
          ConversionID(Cia:Tced:Tcns);                                                              
          Clear Nro;                                                                                
          Exec Sql                                                                                  
           Select Count(0) Into :Nro                                                                
           From Votrea00/Vtwpgef                                                                    
           Where Pgecia = :Cia And                                                                  
                 Pgecmp = :Cam And                                                                  
                 Trim(Char(Pgecdg)) = Trim(:Tced) And                                               
                 (Pgeest = 'R' Or Pgeest = 'I');                                                    
          If Nro > 0;                                                                               
           Exec Sql                                                                                 
            Update Votrea00/Vtwlpgf Set Lpgdsa = 'SI', Lpgmot = ' '                                 
            Where Lpgcia = :Cia And                                                                 
                  Lpgcmp = :Cam And                                                                 
                  Lpgzna = :Zon And                                                                 
                  Lpgcdg = :Ced And                                                                 
                  Lpgmot <> ' ' And                                                                 
                  Lpgvde > 0;                                                                       
          Else;                                                                                     
           Exec Sql                                                                                 
            Update Votrea00/Vtwlpgf Set Lpgdsa = 'NO',                                              
            Lpgmot = 'Ajuste no efectuado por concepto efectivo'                                    
            Where Lpgcia = :Cia And                                                                 
                  Lpgcmp = :Cam And                                                                 
                  Lpgzna = :Zon And                                                                 
                  Lpgcdg = :Ced And                                                                 
                  Lpgvde > 0;                                                                       
          Endif;                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
                                                                                                    
        If qLpgDSA = 'NO' And qLpgmot = ' ';                                                        
         Clear Wmotivo;                                                                             
         Exec Sql                                                                                   
          Select Pcgmot Into :Wmotivo                                                               
          From Votrea00/Vtwpcgf                                                                     
          Where Pcgcia = :Cia And                                                                   
                Pcgcmp = :Cam And                                                                   
                Pcgcdg = :Ced And                                                                   
                Pcgind = 'N'                                                                        
          Fetch First 1 Row Only;                                                                   
                                                                                                    
         If Wmotivo = ' ';                                                                          
          Exec Sql                                                                                  
           Select Pprdes Into :Wmotivo                                                              
           From Votrea00/Vttpprf                                                                    
           Where Pprcia = :Cia And                                                                  
                 Pprcmp = :Cam And                                                                  
                 Pprcdg = :Ced                                                                      
           Fetch First 1 Row Only;                                                                  
         Endif;                                                                                     
                                                                                                    
         If Wmotivo <> ' ';                                                                         
          Exec Sql                                                                                  
           Update Votrea00/Vtwlpgf Set Lpgmot = :Wmotivo                                            
           Where Lpgcia = :Cia And                                                                  
                 Lpgcdg = :Ced And                                                                  
                 Lpgcmp = :Cam;                                                                     
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
        Exec SQL                                                                                    
         Fetch Cursor3I Into :Fields;                                                               
       Enddo;                                                                                       
                                                                                                    
       Exec SQL                                                                                     
        Close cursor3I;                                                                             
      /End-Free                                                                                     
     PPrProceso        E                                                                            
                                                                                                    
      *------------------------------------------------------------------------                     
      * Verifica Pagos Realizados                                                                   
      *------------------------------------------------------------------------                     
     PPrValidarPagos   B                                                                            
     DPrValidarPagos   Pi                                                                           
     DWfechaE          s             10    Inz(*Blanks)                                             
     DWfechaC          s             10    Inz(*Blanks)                                             
     DCmdData          s            500a   Inz(*Blanks)                                             
      /Free                                                                                         
       Clear CmdData;                                                                               
       If Compania = '001';                                                                         
       CmdData = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + Compania +                                      
                 '/VLARDTL01) ' +                                                                   
                 'OVRSCOPE(*JOB)';                                                                  
       Else;                                                                                        
       CmdData = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + Compania + '/PARDTL) '                          
                 + 'OVRSCOPE(*JOB)';                                                                
       Endif;                                                                                       
       CallP(E) Comando(CmdData: %Size(CmdData));                                                   
       PCamE= ICam;                                                                                 
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       Pnume='001';                                                                                 
       ProCampa(Compania:PCamE:PCamR:POper:Pnume);                                                  
       If PCamR = '2009';                                                                           
        PCamR = '2008';                                                                             
       Endif;                                                                                       
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select * '+                                                                        
                'From Votrea00/VTWLPGF ' +                                                          
                'Where LPGCIA = ' + '''' + Compania  + '''' + ' And ' +                             
                'LPGCMP = ' + '''' + %TRIM(PCamR) + '''' + ' And ' +                                
                '((Lpgfde = ' + '''' + ' ' + '''' + ' And ' +                                       
                'Lpgvde <> 0) Or (' +                                                               
                'Lpgfdc = ' + '''' + ' ' + '''' + ' And ' +                                         
                'Lpgvdc <> 0)) And Lpgdsa = ' + '''' + 'SI' + '''';                                 
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3p From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor3p Cursor For Cur3p;                                                          
                                                                                                    
       Exec SQL                                                                                     
        Open cursor3p;                                                                              
                                                                                                    
       Exec SQL                                                                                     
        Fetch Cursor3p Into :Fields;                                                                
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= 0;                                                           
        //Fecha Pago en Efectivo                                                                    
        Clear WfechaE;                                                                              
        Clear WfechaC;                                                                              
        PCamE = qLpgcmp;                                                                            
        PCamR = *Blanks;                                                                            
        POper = '+';                                                                                
        Pnume = '001';                                                                              
        ProCampa(qLpgcia:PCamE:PCamR:POper:Pnume);                                                  
        If PCamR = '2009';                                                                          
         PCamR = '2010';                                                                            
        Endif;                                                                                      
        //Fecha Pago en Efectivo                                                                    
        If qLpgfde = ' ' And qLpgvde > 0;                                                           
         Exec Sql                                                                                   
          Select Max(Digits(Adcrcc)||Digits(Adcryy)||'/'||                                          
                 Digits(Adcrmm)||'/'||Digits(Adcrdd))                                               
                 Into :WfechaE                                                                      
          From Pardtl                                                                               
          Where AdcstÑ = :qLpgcdg And (AdtndÑ Like                                                  
          '%DctoCdLE'||Trim(:PCamR)||'mt'||Trim(:PCamE)||'%');                                      
        Endif;                                                                                      
        //Fecha Pago en Cartera                                                                     
        If qLpgfdc = ' ' And qLpgvdc > 0;                                                           
         Exec Sql                                                                                   
          Select Max(Digits(Adcrcc)||Digits(Adcryy)||'/'||                                          
                 Digits(Adcrmm)||'/'||Digits(Adcrdd))                                               
                 Into :WfechaC                                                                      
          From Pardtl                                                                               
          Where AdcstÑ = :qLpgcdg And (AdtndÑ Like                                                  
          '%DctoCdLP'||Trim(:PCamR)||'mt'||Trim(:PCamE)||'%');                                      
        Endif;                                                                                      
        If WfechaE <> ' ' And qLpgfde = ' ' And qLpgvde > 0;                                        
         Exec Sql                                                                                   
          Update Votrea00/Vtwlpgf Set Lpgfde = :WfechaE                                             
          Where Lpgcia = :Compania And                                                              
                Lpgcdg = :qLpgcdg And                                                               
                Lpgcmp = :qLPGCMP;                                                                  
        Endif;                                                                                      
        If WfechaC <> ' ' And qLpgfdc = ' ' And qLpgvdc > 0;                                        
         WfechaC = %ScanRpl('-':'/':WfechaC);                                                       
         Exec Sql                                                                                   
          Update Votrea00/Vtwlpgf Set LPGFDC = :WfechaC,                                            
                                      LPGFVR = :WfechaC                                             
          Where Lpgcia = :Compania And                                                              
                Lpgcdg = :qLpgcdg And                                                               
                Lpgcmp = :qLPGCMP;                                                                  
        Endif;                                                                                      
                                                                                                    
                                                                                                    
        Exec SQL                                                                                    
         Fetch Cursor3p Into :Fields;                                                               
       Enddo;                                                                                       
                                                                                                    
       Exec SQL                                                                                     
        Close cursor3p;                                                                             
                                                                                                    
       Clear CmdData;                                                                               
       CmdData = 'DLTOVR FILE(PARDTL) LVL(*JOB)';                                                   
       CallP(E) Comando(CmdData: %Size(CmdData));                                                   
      /End-Free                                                                                     
     PPrValidarPagos   E                                                                            
                                                                                                    
      *------------------------------------------------------------------------                     
      * Verifica Pagos Realizados                                                                   
      *------------------------------------------------------------------------                     
     PPrValidarPagosT  B                                                                            
     DPrValidarPagosT  Pi                                                                           
     DWfechaE          s             10    Inz(*Blanks)                                             
     DWfechaC          s             10    Inz(*Blanks)                                             
     DCmdData          s            500a   Inz(*Blanks)                                             
     DValor            s             11  2 Inz(0)                                                   
      /Free                                                                                         
       Clear CmdData;                                                                               
       If Compania = '001';                                                                         
       CmdData = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + Compania +                                      
                 '/VLARDTL01) ' +                                                                   
                 'OVRSCOPE(*JOB)';                                                                  
       Else;                                                                                        
       CmdData = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + Compania + '/PARDTL) '                          
                 + 'OVRSCOPE(*JOB)';                                                                
       Endif;                                                                                       
       CallP(E) Comando(CmdData: %Size(CmdData));                                                   
       PCamE= ICam;                                                                                 
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       Pnume='001';                                                                                 
       ProCampa(Compania:PCamE:PCamR:POper:Pnume);                                                  
        If PCamR = '2009';                                                                          
         PCamR = '2008';                                                                            
        Endif;                                                                                      
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select * '+                                                                        
                'From Votrea00/VTWLPGF ' +                                                          
                'Where LPGCIA = ' + '''' + Compania  + '''' + ' And ' +                             
                'LPGCMP = ' + '''' + %TRIM(PCamR) + '''';                                           
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3q From :SqlStr;                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor3q Cursor For Cur3q;                                                          
                                                                                                    
       Exec SQL                                                                                     
        Open cursor3q;                                                                              
                                                                                                    
       Exec SQL                                                                                     
        Fetch Cursor3q Into :Fields;                                                                
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= 0;                                                           
        Clear Valor;                                                                                
        Valor = Qlpgvde + Qlpgvdc;                                                                  
                                                                                                    
        If Qlpgtds <> Valor;                                                                        
         Exec Sql                                                                                   
          Update Votrea00/Vtwlpgf Set Lpgtds= :Valor                                                
          Where Lpgcia = :Compania And                                                              
                Lpgcdg = :qLpgcdg And                                                               
                Lpgcmp = :qLPGCMP;                                                                  
        Endif;                                                                                      
                                                                                                    
        Exec SQL                                                                                    
         Fetch Cursor3q Into :Fields;                                                               
       Enddo;                                                                                       
                                                                                                    
       Exec SQL                                                                                     
        Close cursor3q;                                                                             
                                                                                                    
       Clear CmdData;                                                                               
       CmdData = 'DLTOVR FILE(PARDTL) LVL(*JOB)';                                                   
       CallP(E) Comando(CmdData: %Size(CmdData));                                                   
      /End-Free                                                                                     
     PPrValidarPagosT  E                                                                            
                                                                                                    
      *------------------------------------------------------------------------                     
      * Verifica Pagos Realizados                                                                   
      *------------------------------------------------------------------------                     
     PPrValidarPagosC  B                                                                            
     DPrValidarPagosC  Pi                                                                           
     DWfechaE          s             10    Inz(*Blanks)                                             
     DWfechaC          s             10    Inz(*Blanks)                                             
     DCmdData          s            500a   Inz(*Blanks)                                             
     DSpgcia           s              3a                                                            
     DSpgzon           s              3a                                                            
     DSpgcam           s              4a                                                            
     DSpgced           s              9  0                                                          
     DSpgsec           s              1a                                                            
     DSpgccr           s              3  0                                                          
     DSpgvds           s             11  2                                                          
     DSpginp           s              1a                                                            
     DCvalor           s             11  2                                                          
     DCvalorp          s             11  2                                                          
     DEvalor           s             11  2                                                          
     DKnro             s              3  0                                                          
     DJcon             s              3  0                                                          
     DJcon0            s              3  0                                                          
     DFsw              s               n    Inz(*Off)                                               
      /Free                                                                                         
       Clear CmdData;                                                                               
       If Compania = '001';                                                                         
       CmdData = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + Compania +                                      
                 '/VLARDTL01) ' +                                                                   
                 'OVRSCOPE(*JOB)';                                                                  
       Else;                                                                                        
       CmdData = 'OVRDBF FILE(PARDTL) TOFILE(OPF' + Compania + '/PARDTL) '                          
                 + 'OVRSCOPE(*JOB)';                                                                
       Endif;                                                                                       
       CallP(E) Comando(CmdData: %Size(CmdData));                                                   
       PCamE= ICam;                                                                                 
       PCamR=*Blanks;                                                                               
       POper='-';                                                                                   
       Pnume='001';                                                                                 
       ProCampa(Compania:PCamE:PCamR:POper:Pnume);                                                  
       If PCamR = '2009';                                                                           
        PCamR = '2008';                                                                             
       Endif;                                                                                       
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select Spgcia, Spgzon, Spgcam, Spgced, Spgsec, Spgccr,' +                          
                'Spgvds, Spginp ' +                                                                 
                'From Votrea00/Vtmspgf ' +                                                          
                'Where Spgcia = ' + '''' + Compania  + '''' + ' And ' +                             
                'Spgcam = ' + '''' + %Trim(PCamR) + '''' + ' And ' +                                
                'Spgvds <> 0 And Spgvps = 0 And Spgnbt <> ' + '''' +                                
                ' ' + '''';                                                                         
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur3p1 From :SqlStr;                                                                
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor3p1 Cursor For Cur3p1;                                                        
                                                                                                    
       Exec SQL                                                                                     
        Open cursor3p1;                                                                             
                                                                                                    
       Exec SQL                                                                                     
        Fetch Cursor3p1 Into :Spgcia, :Spgzon, :Spgcam, :Spgced, :Spgsec,                           
                             :Spgccr, :Spgvds, :Spginp;                                             
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= 0;                                                           
       Clear Knro;                                                                                  
       Exec Sql                                                                                     
        Select Count(0) Into :Knro                                                                  
        From Votrea00/Vtmspgf                                                                       
        Where Spgcia = :Spgcia And                                                                  
              Spgced = :Spgced And                                                                  
              Spgcam = :Spgcam And                                                                  
              Spgzon = :Spgzon And                                                                  
              Spgccr = :Spgccr;                                                                     
                                                                                                    
       If Knro <=1;                                                                                 
        PCamE = Spgcam;                                                                             
        PCamR = *Blanks;                                                                            
        POper = '+';                                                                                
        Pnume = '001';                                                                              
        ProCampa(Spgcia:PCamE:PCamR:POper:Pnume);                                                   
        If PCamR = '2009';                                                                          
         PCamR = '2010';                                                                            
        Endif;                                                                                      
        //Pago en Efectivo                                                                          
        Clear Cvalor;                                                                               
        Clear Cvalorp;                                                                              
        Clear Evalor;                                                                               
        If Spgccr = 043;                                                                            
         Exec Sql                                                                                   
          Select Sum(Adtram) Into :Evalor                                                           
          From Pardtl                                                                               
          Where AdcstÑ = :Spgced And (AdtndÑ Like                                                   
          '%DctoCdLE'||Trim(:PCamR)||'mt'||Trim(:PCamE)||'%') And                                   
          Adrdpt = 043 And Adtram > 0;                                                              
        Endif;                                                                                      
        //Pago en Cartera                                                                           
        If Spgccr = 042;                                                                            
         Exec Sql                                                                                   
          Select Sum(Adtram) * (-1) Into :Cvalor                                                    
          From Pardtl                                                                               
          Where AdcstÑ = :Spgced And (AdtndÑ Like                                                   
          '%DctoCdLP'||Trim(:PCamR)||'mt'||Trim(:PCamE)||'%') And                                   
          Adrdpt = 042 And AdtndÑ Not Like '%COMPRAYA%';                                            
        Endif;                                                                                      
        If Spgvds <> Cvalor And Cvalor <> 0;                                                        
         Fsw = *On;                                                                                 
         Jcon = 0;                                                                                  
         Exec Sql                                                                                   
          Select Count(0) Into :Jcon                                                                
          From Votrea00/Vtmspgf                                                                     
          Where Spgcia =:Spgcia And                                                                 
                Spgced =:Spgced And                                                                 
                Spgcam =:Spgcam And                                                                 
                Spgccr = 042;                                                                       
         If Jcon <= 1;                                                                              
         Exec Sql                                                                                   
          Update Votrea00/Vtmspgf Set Spgvds = :Cvalor                                              
          Where Spgcia = :Spgcia And                                                                
                Spgced = :Spgced And                                                                
                Spgcam = :Spgcam And                                                                
                Spgzon = :Spgzon And                                                                
                Spgsec = :Spgsec And                                                                
                Spgccr = 042;                                                                       
         Endif;                                                                                     
         Endif;                                                                                     
                                                                                                    
        If Spgvds <> Evalor And Evalor <> 0;                                                        
         Fsw = *On;                                                                                 
         Jcon0 = 0;                                                                                 
         Exec Sql                                                                                   
          Select Count(0) Into :Jcon0                                                               
          From Votrea00/Vtmspgf                                                                     
          Where Spgcia =:Spgcia And                                                                 
                Spgced =:Spgced And                                                                 
                Spgcam =:Spgcam And                                                                 
                Spgccr = 043;                                                                       
         If Jcon0 <= 1;                                                                             
         Exec Sql                                                                                   
          Update Votrea00/Vtmspgf Set Spgvds = :Evalor                                              
          Where Spgcia = :Spgcia And                                                                
                Spgced = :Spgced And                                                                
                Spgcam = :Spgcam And                                                                
                Spgzon = :Spgzon And                                                                
                Spgsec = :Spgsec And                                                                
                Spgccr = 043;                                                                       
         Endif;                                                                                     
         Endif;                                                                                     
                                                                                                    
        If Spgccr = 042 And                                                                         
           Spgvds <> Cvalor And Cvalor <> 0;                                                        
         Clear Cvalorp;                                                                             
         Cvalorp = Cvalor;                                                                          
         If Spginp = 'E';                                                                           
          Clear CvalorP;                                                                            
         Endif;                                                                                     
         Fsw = *On;                                                                                 
         Exec Sql                                                                                   
          Update Votrea00/Vtwlpgf set LPGVDC =:Cvalor,                                              
                                      LPGVRC = :CvalorP                                             
          Where Lpgcia = :Spgcia And                                                                
                Lpgcdg = :Spgced And                                                                
                Lpgcmp = :Spgcam And                                                                
                Lpgzna = :Spgzon And                                                                
                Lpgsec = :Spgsec;                                                                   
        Endif;                                                                                      
                                                                                                    
        If Spgccr = 043 And                                                                         
           Spgvds <> Evalor And Evalor <> 0;                                                        
         Fsw = *On;                                                                                 
         Exec Sql                                                                                   
          Update Votrea00/Vtwlpgf set LPGVDE =:Evalor                                               
          Where Lpgcia = :Spgcia And                                                                
                Lpgcdg = :Spgced And                                                                
                Lpgcmp = :Spgcam And                                                                
                Lpgzna = :Spgzon And                                                                
                Lpgsec = :Spgsec;                                                                   
        Endif;                                                                                      
        If Fsw = *On;                                                                               
         Fsw = *Off;                                                                                
         Exec Sql                                                                                   
          Update Votrea00/Vtwlpgf set LPGTDS = LPGVDC + LPGVDE                                      
          Where Lpgcia = :Spgcia And                                                                
                Lpgcdg = :Spgced And                                                                
                Lpgcmp = :Spgcam And                                                                
                Lpgzna = :Spgzon And                                                                
                Lpgsec = :Spgsec;                                                                   
        Endif;                                                                                      
        Endif;                                                                                      
        Exec SQL                                                                                    
         Fetch Cursor3p1 Into :Spgcia, :Spgzon, :Spgcam, :Spgced, :Spgsec,                          
                              :Spgccr, :Spgvds, :Spginp;                                            
       Enddo;                                                                                       
                                                                                                    
       Exec SQL                                                                                     
        Close cursor3p1;                                                                            
                                                                                                    
       Clear CmdData;                                                                               
       CmdData = 'DLTOVR FILE(PARDTL) LVL(*JOB)';                                                   
       CallP(E) Comando(CmdData: %Size(CmdData));                                                   
      /End-Free                                                                                     
     PPrValidarPagosC  E                                                                            
                                                                                                    
      *------------------------------------------------------------------------                     
      * Verifica Pagos Realizados                                                                   
      *------------------------------------------------------------------------                     
     PPrFileNewxx      B                                                                            
     DPrFileNewxx      Pi                                                                           
     DLpgciaQ          s              3a                                                            
     DLpgznaQ          s              3a                                                            
     DLpgcmpQ          s              4a                                                            
     DLpgsecQ          s              1a                                                            
     DLpgcdgQ          s              9  0                                                          
      /Free                                                                                         
       // Pagos en Cartera                                                                          
       Exec Sql                                                                                     
        Declare CurT0 Cursor For                                                                    
        Select Lpgcia, Lpgzna, Lpgcmp, Lpgsec, Lpgcdg                                               
        From Votrea00/Vtwlpgf Inner Join Votrea00/Vtmspgf On                                        
        Spgcia = Lpgcia And                                                                         
        Spgzon = Lpgzna And                                                                         
        Spgcam = Lpgcmp And                                                                         
        Spgsec = Lpgsec And                                                                         
        Spgced = Lpgcdg                                                                             
        Where Lpgdsa = 'SI'                                                                         
        And Lpgfvr = ' '                                                                            
        And Spgvps <> 0                                                                             
        And Lpgcmp >= '2101'                                                                        
        And Spginp = ' '                                                                            
        And Spgccr = 042;                                                                           
                                                                                                    
       Exec Sql                                                                                     
        Open CurT0;                                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Fetch CurT0 Into :LpgciaQ, :LpgznaQ, :LpgcmpQ, :LpgsecQ, :LpgcdgQ;                          
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Clear SqlStr;                                                                               
        SqlStr = 'Update ' + %Trim(User) + '/VTWLPGF ' +                                            
                 'Set Lpgdsa = ' + '''' + 'NO' + '''' + ', ' +                                      
                 'Lpgmot = ' + '''' +                                                               
                 'Ajuste no efectuado por concepto cartera' + '''' + ' ' +                          
                 'Where Lpgcia = ' + '''' + LpgciaQ + '''' + ' And ' +                              
                 'Lpgcmp = ' + '''' + LpgcmpQ + '''' + ' And ' +                                    
                 'Lpgzna = ' + '''' + LpgznaQ + '''' + ' And ' +                                    
                 'Lpgsec = ' + '''' + LpgsecQ + '''' + ' And ' +                                    
                 'Lpgcdg = ' + %Trim(%Editc(LpgcdgQ:'Z'));                                          
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
        Exec Sql                                                                                    
         Fetch CurT0 Into :LpgciaQ, :LpgznaQ, :LpgcmpQ, :LpgsecQ, :LpgcdgQ;                         
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CurT0;                                                                                
                                                                                                    
       // Pagos en Efectivo                                                                         
       Exec Sql                                                                                     
        Declare CurT1 Cursor For                                                                    
        Select Lpgcia, Lpgzna, Lpgcmp, Lpgsec, Lpgcdg                                               
        From Votrea00/Vtwlpgf Inner Join Votrea00/Vtmspgf On                                        
        Spgcia = Lpgcia And                                                                         
        Spgzon = Lpgzna And                                                                         
        Spgcam = Lpgcmp And                                                                         
        Spgsec = Lpgsec And                                                                         
        Spgced = Lpgcdg                                                                             
        Where Lpgdsa = 'SI'                                                                         
        And Lpgfvr = ' '                                                                            
        And Spgvps <> 0                                                                             
        And Lpgcmp >= '2101'                                                                        
        And Spginp = ' '                                                                            
        And Spgccr = 043;                                                                           
                                                                                                    
       Exec Sql                                                                                     
        Open CurT1;                                                                                 
                                                                                                    
       Exec Sql                                                                                     
        Fetch CurT1 Into :LpgciaQ, :LpgznaQ, :LpgcmpQ, :LpgsecQ, :LpgcdgQ;                          
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
        Clear SqlStr;                                                                               
        SqlStr = 'Update ' + %Trim(User) + '/VTWLPGF ' +                                            
                 'Set Lpgdsa = ' + '''' + 'NO' + '''' + ', ' +                                      
                 'Lpgmot = ' + '''' +                                                               
                 'Ajuste no efectuado por concepto efectivo' + '''' + ' ' +                         
                 'Where Lpgcia = ' + '''' + LpgciaQ + '''' + ' And ' +                              
                 'Lpgcmp = ' + '''' + LpgcmpQ + '''' + ' And ' +                                    
                 'Lpgzna = ' + '''' + LpgznaQ + '''' + ' And ' +                                    
                 'Lpgsec = ' + '''' + LpgsecQ + '''' + ' And ' +                                    
                 'Lpgcdg = ' + %Trim(%Editc(LpgcdgQ:'Z'));                                          
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
        Exec Sql                                                                                    
         Fetch CurT1 Into :LpgciaQ, :LpgznaQ, :LpgcmpQ, :LpgsecQ, :LpgcdgQ;                         
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close CurT1;                                                                                
      /End-Free                                                                                     
     PPrFileNewxx      E                                                                            
