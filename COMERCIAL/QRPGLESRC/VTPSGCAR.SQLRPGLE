      ***********************************************************************                       
      ** Programa      : VTPSGCAR                                          **                       
      ** Descripciòn   : PGM Consulta Control de Pagos Guías V2            **                       
      ** Proyecto      : Plan de Guías                                     **                       
      ** Autor         : (AParraQ)                                         **                       
      ** Fecha Creación: Marzo de 2020                                     **                       
      ***********************************************************************                       
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
     Hdatfmt(*Iso)                                                                                  
                                                                                                    
     **Definicion de informes                                                                       
     FVTCCCP0P  Cf   e             Workstn                                                          
     FVTWLPGF   IF   E           K DISK    Prefix(Q)                                                
                                                                                                    
      **Definición de Variables de Trabajo                                                          
     DUser             s             10a   Inz(*User)                                               
     D$C               c                   ''''                                                     
     DSqlStr           s           3000a   Inz(*Blanks)                                             
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
     DWcamp                           4a   Inz(*Blanks)                                             
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
                                                                                                    
      **Estructura para los indicadores de pantalla                                                 
     Dptr              S               *   Inz(%addr(*In))                                          
     D@ind             DS                  Based(ptr)                                               
     D F3Salir                         N   Overlay(@Ind:03)                                         
     D F5GenerarInfo                   N   Overlay(@Ind:05)                                         
     D F12Cancelar                     N   Overlay(@Ind:12)                                         
     D PosicionarCur                   N   Overlay(@Ind:31)                                         
     D ErrorDatos                      N   Overlay(@Ind:32)                                         
                                                                                                    
                                                                                                    
      **Definición de Procedimientos                                                                
     DPrCreateFile     pr                                                                           
     DPrCentrar        Pr            50A                                                            
     D pCadena                       50A   Const                                                    
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10                                                             
     DPres                            2                                                             
     DKciaR                          10                                                             
                                                                                                    
     DPrCampaniaValida...                                                                           
     D                 Pr              N                                                            
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     Dpaises           Pr                  Extpgm('VPOPIP00/VTOPICNR')                              
     DciaP                            3                                                             
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /free                                                                                         
                                                                                                    
        Dow Not F3Salir;                                                                            
           PTitulo = PrCentrar('Consulta Pagos Guías Resumen');                                     
           Write rBlanco;                                                                           
           exfmt rPrincipal;                                                                        
           ErrorDatos=*off;                                                                         
           Clear PMSJ;                                                                              
           If POPPPAIS=1;                                                                           
              paises(PCIA);                                                                         
              If PCIA<>*Blanks;                                                                     
                 PgmPais(PCIA:PDESPAI);                                                             
                 PosicionarCur=*on;                                                                 
              Else;                                                                                 
                 PosicionarCur=*off;                                                                
              EndIf;                                                                                
              POPPPAIS=*Zeros;                                                                      
              Iter;                                                                                 
           EndIf;                                                                                   
           If F3Salir;                                                                              
              leave;                                                                                
           EndIf;                                                                                   
           If F12Cancelar;                                                                          
              clear rPrincipal;                                                                     
              F12Cancelar=*off;                                                                     
              iter;                                                                                 
           EndIf;                                                                                   
           If F5GenerarInfo;                                                                        
              If PCIA<>*Blanks;                                                                     
                 *in30=*off;                                                                        
                 If PCAMPA<>*Blanks;                                                                
                    PosicionarCur=*off;                                                             
                    If PrCampaniaValida();                                                          
                       PrCreateFile();                                                              
                       Pr_CargarIformacionAlter();                                                  
                       Pfile = 'VTHLPGF';                                                           
                       Plib = User;                                                                 
                       Write rBlanco;                                                               
                       exfmt rResulta;                                                              
                       clear rPrincipal;                                                            
                    Else;                                                                           
                       PosicionarCur=*on;                                                           
                       ErrorDatos=*on;                                                              
                       PMSJ = PrCentrar('No se Encontró Información para la ' +                     
                              'Consulta');                                                          
                       Iter;                                                                        
                    EndIf;                                                                          
                 Else;                                                                              
                   ErrorDatos=*on;                                                                  
                   PMSJ = PrCentrar('Ingrese la campaña para generar el ' +                         
                         'informe');                                                                
                   Iter;                                                                            
                 EndIf;                                                                             
              Else;                                                                                 
                 ErrorDatos=*on;                                                                    
                 PMSJ = PrCentrar('Seleccione la compañia para generar el ' +                       
                        'informe');                                                                 
                 Iter;                                                                              
              EndIf;                                                                                
              F5GenerarInfo=*off;                                                                   
           EndIf;                                                                                   
        EndDo;                                                                                      
        *inlr=*on;                                                                                  
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
      *-------------------------------------------------------------------------                    
      * Valida la existencia de información de la Campaña y Compañia ingresada                      
      *-------------------------------------------------------------------------                    
     PPrCampaniaValida...                                                                           
     P                 B                                                                            
     D                 PI              N                                                            
     DwContador        s              8S 0 Inz(*Zeros)                                              
      /Free                                                                                         
                                                                                                    
         PCamE= PCAMPA;                                                                             
         PCamR=*Blanks;                                                                             
         POper='-';                                                                                 
         Pnume='001';                                                                               
         proCampa(PCIA:PCamE:PCamR:POper:Pnume);                                                    
                                                                                                    
         Exec Sql Select Count(1)                                                                   
         Into :wContador                                                                            
         From VOTREA00/VTWLPGF                                                                      
         Where LPGCIA = :PCIA And LPGCMP = :PCamR;                                                  
                                                                                                    
         If wContador > 0;                                                                          
            Return *On;                                                                             
         EndIf;                                                                                     
         Return *Off;                                                                               
                                                                                                    
      /End-Free                                                                                     
     PPrCampaniaValida...                                                                           
     P                 E                                                                            
                                                                                                    
                                                                                                    
     P*---------------------------------------------------------                                    
     P* Procedure Name: PrFileNew                                                                   
     P* Purpose       : Graba Información en Archivo de Trabajo                                     
     P*---------------------------------------------------------                                    
     PPrCreateFile     B                                                                            
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
                                                                                                    
       SqlStr = 'Drop Table ' + %Trim(User) + '/VTHLPGF';                                           
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
       SqlStr = 'Delete From ' + %Trim(User) + '/VTHLPGF';                                          
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
       SqlStr = 'Create Table ' + %Trim(User) + '/VTHLPGF As (' +                                   
                'Select LPGCIA, LPGCMP, LPGREG, LPGZNA, LPGSEC, LPGCDG, '+                          
                'LPGNOM, LPGTDS ' +                                                                 
                'From Votrea00/VTWLPGF ' +                                                          
                'Where LPGCIA = ' +$C+ PCIA +$C+ ' And ' +                                          
                'LPGCMP = ' +$C+ %TRIM(PCamR) +$C+ ' And ' +                                        
                'LPGZNA In (Select Estzon From orderp00/Opiestf1 ' +                                
                'Where Estcia= ' +$C+ PCIA +$C+                                                     
                ' And Estcam = ' +$C+ PCAMPA +$C+                                                   
                ' And Estest = ' +$C+'S' +$C+ ')'+                                                  
                ' Order By LPGZNA, LPGSEC) With Data';                                              
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
       If SqlCode < 0;                                                                              
          SqlStr = 'Insert into Table ' + %Trim(User) + '/VTHLPGF (' +                              
                   'Select LPGCIA, LPGCMP, LPGREG, LPGZNA, LPGSEC, LPGCDG, '+                       
                   'LPGNOM, LPGTDS ' +                                                              
                   'From Votrea00/VTWLPGF ' +                                                       
                   'Where LPGCIA = ' +$C+ PCIA +$C+ ' And ' +                                       
                   'LPGCMP = ' +$C+ PCamR +$C+ ' And ' +                                            
                   'LPGZNA In (Select Estzon From orderp00/Opiestf1 ' +                             
                   'Where Estcia= ' +$C+ PCIA +$C+                                                  
                   ' And Estcam = ' +$C+ PCAMPA +$C+                                                
                   ' And Estest = ' +$C+'S' +$C+ ')'+                                               
                   ' Order By LPGZNA, LPGSEC)';                                                     
          Exec Sql                                                                                  
           Execute Immediate :SqlStr;                                                               
       Else;                                                                                        
                                                                                                    
          SqlStr = 'Label on Table ' + %Trim(User) + '/VTHLPGF Is ' +                               
                   '''Consulta Pagos Guías Resumen''';                                              
          Exec Sql                                                                                  
           Execute Immediate :SqlStr;                                                               
                                                                                                    
          SqlStr = ' ALTER TABLE ' + %Trim(User) + '/VTHLPGF'+                                      
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
                   ' ADD COLUMN LPGIRV CHARACTER(13) NOT HIDDEN';                                   
          Exec Sql                                                                                  
           Execute Immediate :SqlStr;                                                               
                                                                                                    
          //* Estableciendo las etiquetas de columna para USER.VTWLPGF */                           
          SqlStr = 'LABEL ON COLUMN '+%Trim(User)+'/VTHLPGF ( '+                                    
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
               ' LPGIRV TEXT IS '+ $C +'Inscrip Rent Valor'+ $C +'  )';                             
          Exec Sql                                                                                  
           Execute Immediate :SqlStr;                                                               
                                                                                                    
                  //* Estableciendo las cabeceras de columna para USER.VTWLPGF */                   
          SqlStr ='LABEL ON COLUMN '+ %Trim(User)+'/VTHLPGF ('+                                     
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
                ' LPGIRV IS '+ $C +'Inscrip Rent Vlr    '+ $C  +'  )';                              
          Exec Sql                                                                                  
           Execute Immediate :SqlStr;                                                               
       EndIf;                                                                                       
                                                                                                    
      /End-Free                                                                                     
     PPrCreateFile     E                                                                            
                                                                                                    
     P*---------------------------------------------------------                                    
     P* Procedure Name: Pr_CargarIformacionAlter                                                    
     P* Purpose       : Recorre archivo pago lideres VtwLpgf                                        
     P*---------------------------------------------------------                                    
     PPr_CargarIformacionAlter...                                                                   
     P                 B                                                                            
     DPr_CargarIformacionAlter...                                                                   
     D                 PI                                                                           
     Dw_Zona           s              3    Inz(*Blanks)                                             
      ** Definición de Variables de Trabajo                                                         
      /Free                                                                                         
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Select distinct LPGZNA From ' + %Trim(User) + '/VTHLPGF '+                         
                'Order By LPGZNA';                                                                  
           Exec Sql Prepare C0 From :SqlStr;                                                        
           Exec Sql Declare C0 Cursor For C0;                                                       
           Exec Sql Open C0;                                                                        
           Exec Sql fetch C0 Into  :w_Zona;                                                         
           Dow SqlCod = *Zeros;                                                                     
               Clear DsResultadosGu;                                                                
               Clear DsDetalleGuias;                                                                
               Clear *all DsResultados;                                                             
               SPcia = PCIA;                                                                        
               Pres = *Blanks;                                                                      
               PciaR = *Blanks;                                                                     
               CompaÑia(SPcia:Pres:PciaR);                                                          
               WCIA  =   PciaR;                                                                     
               WZONA =   w_Zona;                                                                    
               WCAMP =   PCAMPA;                                                                    
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
                           %Trim(User) + '/VTHLPGF '+                                               
                           ' where Lpgzna = ' + $C + w_Zona  + $C +                                 
                           '   And Lpgcia = ' + $C +%trim(PCIA) + $C +                              
                           '   And Lpgcmp = ' + $C +%trim(PCamR) + $C ;                             
                                                                                                    
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
                                                                                                    
                       Clear  SqlStr;                                                               
                       SqlStr = ' Update ' + %Trim(User) + '/VTHLPGF Set '+                         
                     ' Lpgmpd = ' + $C + %Trim(%Editc(Imetpeda:'Z')) + $C + ','+                    
                     ' Lpgpdt = ' + $C + %Trim(%Editc(Ipedtota:'Z')) + $C + ','+                    
                     ' Lpgcmt = ' + $C + %Trim(Icummet )             + $C + ','+                    
                     ' Lpgptp = ' + $C + %Trim(%Editc(Ipedpaga:'Z')) + $C + ','+                    
                     ' Lpgcbs = ' + $C + %Trim(%Editc(Ipedbas:'Z'))  + $C + ',';                    
                                                                                                    
                %Occur(DsDetalleR)   = 1;                                                           
                SqlStr = %Trim(SqlStr ) +                                                           
                     ' Lpgppc = ' + %Trim(%Editc(RCantidad:'P')) + ',' +                            
                     ' Lpgppv = ' + $C + %Trim(RValor) + $C + ',' ;                                 
                %Occur(DsDetalleR)   = 2;                                                           
                SqlStr = %Trim(SqlStr ) +                                                           
                     ' Lpgppa = ' + %Trim(%Editc(RCantidad:'P')) + ',' +                            
                     ' Lpgpav = ' + $C + %Trim(RValor) + $C + ',' +                                 
                     ' Lpginc = ' + $C + %Trim(%Editc(Iinstota:'P')) + $C + ','+                    
                     ' Lpginr = ' + $C + %Trim(%Editc(Iinsdela:'P')) + $C + ',';                    
                %Occur(DsDetalleR)   = 3;                                                           
                SqlStr = %Trim(SqlStr ) +                                                           
                     ' Lpgirv = ' + $C + %Trim(RValor) + $C +                                       
                     ' where Lpgzna = ' + $C + w_Zona         + $C +                                
                     ' And Lpgcia = ' + $C +%trim(PCIA) + $C +                                      
                     ' And Lpgcdg = ' + %Char(Qlpgcdg)  +                                           
                     ' And Lpgsec = ' + $C +%trim(Isector) + $C +                                   
                     ' And Lpgcmp = ' + $C +%trim(PCamR)    + $C ;                                  
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
