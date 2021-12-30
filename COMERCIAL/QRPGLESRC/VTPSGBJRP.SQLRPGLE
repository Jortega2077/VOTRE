      **********************************************************************                        
      ** Programa     : VTPSGBJR                                          **                        
      ** Descripciòn: : Pg Cálculo de Valor a Pagar a una Guía por Campaña**                        
      ** Proyecto     : Plan Guias                                        **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Creaciòn: 14 de Junio de 2016                              **                        
      **********************************************************************                        
      ** Descripciòn: : Insertar información a Archovo Votrea00/VtwLpgf   **                        
      **                Archivo de control de pagos Gías                  **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Modificación: 30 de Marzo de 2017                          **                        
      **********************************************************************                        
      ** Descripciòn: : Calcular Valores Retención Guías                  **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Fecha Modificación: 05 de Junio de 2017                          **                        
      **********************************************************************                        
      ** Descripciòn: : Recupera valor pago en cartera plan alas          **                        
      ** Autor        : Leonel Mauricio Parra Suárez-PersonalSoft         **                        
      ** Responsable  : JOrtega                     -Leonisa S.A.S.       **                        
      ** Fecha Modificación: 20 de Junio de 2019                          **                        
      **********************************************************************                        
     HOption(*NoDebugIo)                                                                            
     HDFTACTGRP(*NO) ACTGRP(*NEW) BNDDIR('QC2LE')                                                   
                                                                                                    
     ** Definición de Prototipos de Trabajo                                                         
     FVtmspgf   UF A E           K DISK                                                             
     FVtospgf   UF A E           K DISK    Rename(Regspg:Rgspg) Prefix(W)                           
     FVttpprf   Uf a e           k Disk                                                             
     FVttcpgf   Uf a e           k Disk                                                             
     FVtwVrff   UF A E           K DISK                                                             
     FVtwcphf   UF A E           K DISK    USROPN                                                   
     FVTMEGMF   IF   E           K DISK                                                             
                                                                                                    
     DFields         e Ds                  Extname(VtwLpgf:RegLpg)                                  
     D                                     Prefix(Q)                                                
     DFields1        e Ds                  Extname(VtwLpgf:RegLpg)                                  
     D                                     Prefix(Y)                                                
     DFields2        e Ds                  Extname(VtwCpgf:RegCpg)                                  
     D                                     Prefix(Q_)                                               
     DFields3        e Ds                  Extname(pardtl:PARDTLF1)                                 
     D                                     Prefix(Q)                                                
     DFields4        e Ds                  Extname(Vtmspgf:REGSPG)                                  
     D                                     Prefix(Q)                                                
     ** Definiciòn de Variables de Trabajo                                                          
     DUser             s             10    Inz(*User)                                               
     DTTsw             s               n   Inz(*Off)                                                
     DVrgest           S              1    Inz(' ')                                                 
     DNroC             S              9  0 Inz(0)                                                   
     DTnroP            S              9  0 Inz(0)                                                   
                                                                                                    
     DMain             Pr                  Extpgm('VTPSGBJR')                                       
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DIcia                            3a                                                            
     DICamPa                          4A                                                            
     DIZona                           3a                                                            
                                                                                                    
     DConversion       Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DYcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
      *Definición de Procedimientos                                                                 
                                                                                                    
     DCAMBIARCq        PR                  Extpgm('VOTREP00/VTMGOIQR')                              
     DWADTNDÑ                        30                                                             
                                                                                                    
     DProCampa         Pr                  Extpgm('VOTREP00/VTMCSBKR')                              
     DPCia                            3a                                                            
     DPCamE                           4a                                                            
     DPCamR                           4a                                                            
     DPOper                           1a                                                            
     DPnume                           3a                                                            
                                                                                                    
      * Programa validar region                                                                     
     DPgmregio         Pr                  Extpgm('VOTREP00/VTMGOIGR')                              
     DIcia                            3a                                                            
     DIzona                           3a                                                            
     DORegion                        30a                                                            
     DOced                           10A                                                            
     DOplan                          10a                                                            
                                                                                                    
      *Programa Valida Pago en Efectivo                                                             
     DPgmpgrEfec       Pr                  Extpgm('VOTREP00/VTMPGBHR')                              
     DPCia                            3a                                                            
     DPCampa                          4a                                                            
     DPzona                           3a                                                            
     DOValor                         11a                                                            
                                                                                                    
      * Programa validar region                                                                     
     DPgmPoltComer     Pr                  Extpgm('VOTREP00/VTPSGBML')                              
     DIcia                            3a                                                            
     DICampa                          4a                                                            
     DIzona                           3a                                                            
     DICedulaG                        9  0                                                          
     DIsector                         3A                                                            
                                                                                                    
     DPgmEvaluaPoliticasGuias...                                                                    
     D                 Pr                  Extpgm('VOTREP00/VTPSGBOL')                              
     DIcia                            3a                                                            
     DICampa                          4a                                                            
     DIzona                           3a                                                            
     DWGldcteA                        9                                                             
     DIsector                         3A                                                            
                                                                                                    
     DPgmEvaluaPoliticasComerciales...                                                              
     D                 Pr                  Extpgm('VOTREP00/VTPSGBNL')                              
     DIcia                            3a                                                            
     DICampaAct                       4a                                                            
     DICampaAnt                       4a                                                            
     DIzona                           3a                                                            
     DWGldcteA                        9                                                             
     DIsector                         3A                                                            
                                                                                                    
     DPgmMetodo2       Pr                  Extpgm('VOTREP00/VTMPGBKR')                              
     DIcia                            3a                                                            
     DIregion                        30a                                                            
     DIzona                           3a                                                            
     DIsector                         3a                                                            
     DIcampa                          4a                                                            
     DOvalorW                        11  2                                                          
     DOvalorG                        11  2                                                          
                                                                                                    
     DValorConsignar   Pr                  Extpgm('VOTREP00/VTMCSB2R')                              
     DPcia                            3a                                                            
     DIcedula                         9                                                             
     DIValor                         11                                                             
                                                                                                    
     DDsNewPlanGu      Ds                                                                           
     DIcaso                           7a   Inz(*Blanks)                                             
     DIcedula                        30a   Inz(*Blanks)                                             
     DIfecha                         10a   Inz(*Blanks)                                             
     DOrespuesta                      1a   Inz(*Blanks)                                             
                                                                                                    
     DPgmNewPlanG      Pr                  Extpgm('VOTREP00/VTMPGB0R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcaso                           7a                                                            
     DIcedula                        30a                                                            
     DIfecha                         10a                                                            
     DOrespuesta                      1a                                                            
     DPrValorTotPdo    Pr            11  2                                                          
                                                                                                    
     DPgmPoliticasRUT  Pr                  Extpgm('VOTREP00/VTMPGB3R')                              
     DPcia                            3a                                                            
     DIzona                           3a                                                            
     DIcedula                        30a                                                            
     DOrespuestaPgoRUT...                                                                           
     D                                1a                                                            
     DOrespuestaIngCnt...                                                                           
     D                                1a                                                            
                                                                                                    
      *Definición de variables de trabajo                                                           
                                                                                                    
     DNresta           s             11  2 Inz(*Zeros)                                              
     DIIsw             s               n   Inz(*Off)                                                
     DTvalor           s             11  2 Inz(*Zeros)                                              
     DTvalor1          s             11  2 Inz(*Zeros)                                              
     DTvalor2          s             11  2 Inz(*Zeros)                                              
     DNsuma            s             11  2 Inz(*Zeros)                                              
     DPagoG            s           2000a    Inz(*Blanks)                                            
     DWCedreempla      s              9  0  Inz(*Zeros)                                             
     DWCedAux          s              9  0  Inz(*Zeros)                                             
     DW_CampAct        s              4    Inz(*Blanks)                                             
     DW_CampAnt        s              4    Inz(*Blanks)                                             
     DWGldcteA         s              9    Inz(*Blanks)                                             
     DIcedulaSg        s              9    Inz(*Blanks)                                             
     DIValorSg         s             11    Inz(*Blanks)                                             
     DOvalorW          s             11  2 Inz(*Zeros)                                              
     DOvalorG          s             11  2 Inz(*Zeros)                                              
     DPCamE            s              4a   Inz(*Blanks)                                             
     DPCamR            s              4a   Inz(*Blanks)                                             
     DPOper            s              1a   Inz(*Blanks)                                             
     DPnume            s              3a   Inz(*Blanks)                                             
     DWsql             s           1000a   Inz(*Blanks)                                             
     DORegion          s             30A   Inz(*Blanks)                                             
     DOCed             s             10A   Inz(*Blanks)                                             
     DOplan            s             10a   Inz(*Blanks)                                             
     D$COMA            C                   ''''                                                     
     DW_USER           s             10a   Inz(*User)                                               
     DWGldcte          s              9  0 Inz(*Zeros)                                              
     DWGldzon          s              3a   Inz(*Blanks)                                             
     DWGldsct          s              3a   Inz(*Blanks)                                             
     DWGldtrr          s              1a   Inz(*Blanks)                                             
     DW_Valor1         s             11  2 Inz(*Zeros)                                              
     DW_Valor1T        s             11  2 Inz(*Zeros)                                              
     DW_Valor1V        s             11  2 Inz(*Zeros)                                              
     DWValor1          s             11  2 Inz(*Zeros)                                              
     DWValor1T         s             11  2 Inz(*Zeros)                                              
     DWValor1V         s             11  2 Inz(*Zeros)                                              
     DW_Valor2         s             11  2 Inz(*Zeros)                                              
     DW_Valor2T        s             11  2 Inz(*Zeros)                                              
     DW_Valor2V        s             11  2 Inz(*Zeros)                                              
     DWValor2          s             11  2 Inz(*Zeros)                                              
     DWValor2T         s             11  2 Inz(*Zeros)                                              
     DWValor2V         s             11  2 Inz(*Zeros)                                              
     DW_Valor3         s             11  2 Inz(*Zeros)                                              
     DW_Valor3T        s             11  2 Inz(*Zeros)                                              
     DW_Valor3V        s             11  2 Inz(*Zeros)                                              
     DWValor3          s             11  2 Inz(*Zeros)                                              
     DWValor3T         s             11  2 Inz(*Zeros)                                              
     DWValor3V         s             11  2 Inz(*Zeros)                                              
     DX_Valor1         s             11  2 Inz(*Zeros)                                              
     DXValor1          s             11  2 Inz(*Zeros)                                              
     DX_Valor2         s             11  2 Inz(*Zeros)                                              
     DXValor2          s             11  2 Inz(*Zeros)                                              
     DX_Valor3         s             11  2 Inz(*Zeros)                                              
     DXValor3          s             11  2 Inz(*Zeros)                                              
     DW_Cia            s              3a   Inz(*Blanks)                                             
     DW_Zona           s              3a   Inz(*Blanks)                                             
     DW_Camp           s              4a   Inz(*Blanks)                                             
     DOValor           s             11a   Inz(*Blanks)                                             
     DWValor           s             11s 2 Inz(*Zeros)                                              
     DWSpgvds          s             11s 2 Inz(*Zeros)                                              
     DWSpgccr          s              3  0 Inz(*Zeros)                                              
     DWSpginp          s              1a   Inz(*Blanks)                                             
     DySpgfbt          s             10a   Inz(*Blanks)                                             
     Dycpgfbte         s             10a   Inz(*Blanks)                                             
     Dycpgfbtc         s             10a   Inz(*Blanks)                                             
     DW_CampTem        s              4A   Inz(*Blanks)                                             
     DW_salretpen      s              9  0 Inz(45781303)                                            
     DFLAG             s              1s 0 Inz(*Zeros)                                              
     Dwdato            s           5000a                                                            
     DPccte            s             30a   Inz(*Blanks)                                             
     DWADTNDÑ          s             30                                                             
     Dw_AcuDtl         s             15  2                                                          
     Dwx_AcuDtl        s             15  2                                                          
     Dw_TtlPagar       s             11  2                                                          
     Dw_Vrffec         s               z                                                            
     Dw_VRFVRR         s             11  2                                                          
     DV_Valtra         s             11  2 Dim(999)                                                 
     Dw_contatrandtl   s              9  0                                                          
     DW_AcumulaDeuda   s             15  2                                                          
     Dw_ValorRealPagar...                                                                           
     D                 s             11  0 Inz(*Zeros)                                              
     DW_SALDOCOBRAR...                                                                              
     D                 s             11  2 Inz(*Zeros)                                              
     DOrespuestaPgoRUT...                                                                           
     D                 s              1a    Inz('N')                                                
     DOrespuestaIngCnt...                                                                           
     D                 s              1a    Inz('N')                                                
     Dw_VlrPosteDes    s             15  2                                                          
     DHvalor           s             11  2 Inz(*Zeros)                                              
     DKvalor           s             11  2 Inz(*Zeros)                                              
     DTrete            s             11  2 Inz(*Zeros)                                              
     Dw_VlrALiquidado  s             11  2 Inz(*Zeros)                                              
     Dw_SumVlrRet      s             11  2 Inz(*Zeros)                                              
     Dw_SaldoPen       s             11  2 Inz(*Zeros)                                              
     Dw_SumVlrLq       s             11  2 Inz(*Zeros)                                              
     Dw_TemCpgspr      s             15  2 Inz(*Zeros)                                              
     D wx_Vrfipc       s                   Like(Vrfipc)                                             
     Dw_VlrDescu       s             15  2 Inz(*Zeros)                                              
     DW_PorcentaRetencion...                                                                        
     D                 s              6  3 Inz(*Zeros)                                              
     Dw_vlrttl         s             11  2                                                          
     D w_count         s              9  0                                                          
     D w_campAux       s              4                                                             
     DW_sumVds         s             15  2 Inz(*Zeros)                                              
     DW_ccr            s              3  0 Inz(*Zeros)                                              
     DSqlCodAux        s              9  0 Inz(*Zeros)                                              
                                                                                                    
     D                 ds                                                                           
     Dw_WADTNDÑ1               1      4                                                             
     dw_WADTNDÑ2               5      8                                                             
     dw_WADTNDÑ                1      8                                                             
                                                                                                    
     DPrPagosPendientesGuias...                                                                     
     D                 Pr                                                                           
     DPrVtospgf        Pr                                                                           
     DPrVtmipgf        Pr              n                                                            
     DPrVtwlpgf        Pr                                                                           
     dPr_Cal_Retenciones...                                                                         
     D                 Pr                                                                           
     DxNsuma                         11  2                                                          
     DPr_Cal_RetencionesVlrNegativo...                                                              
     D                 Pr                                                                           
     Dw_resultPagoCal                11  2                                                          
     Dw_vlrPagosIn                   11  2                                                          
     DPrValidarPago    Pr              n                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
       Exec Sql                                                                                     
       Delete                                                                                       
       From Votrea00/Vtmipgf                                                                        
       Where Ipgest = 'D';                                                                          
                                                                                                    
       Clear Fields;                                                                                
       W_CampTem = ICamPa;                                                                          
                                                                                                    
       // pgm retorna Región                                                                        
       CallP(E) Pgmregio(ICia:IZona:ORegion:Oced:Oplan);                                            
       //Resta de la  Campaña Cierre de Zona Menos 1 Campañas                                       
       W_CampAct = ICamPa;                                                                          
       PCamE = ICamPa;                                                                              
       PCamR = *Blanks;                                                                             
       POper = '-';                                                                                 
       Pnume = '1';                                                                                 
       If ICamPa = '2010';                                                                          
        Pnume = '002';                                                                              
       Endif;                                                                                       
       callp(E) ProCampa(Icia:PCamE:PCamR:POper:Pnume);                                             
       W_CampAnt = PCamR;                                                                           
       Wsql = *Blanks;                                                                              
       Wsql = 'SELECT Gldcte, Gldzon, Gldsct, Gldtrr ' +                                            
              'FROM VotreA00/Vtmgldf '+                                                             
              'WHERE Gldcia = ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
              'GLDZON = ' + $Coma + %trim(IZona) + $Coma + ' And ' +                                
              '((GLDEST = '' ''' + ' And Gldcin <= ' + '''' +                                       
              PCamR + '''' + ') Or (GLDEST = ''D'' And Gldcin <= ' +                                
              '''' + PCamR + '''' + ' And (' +                                                      
              ' Gldcdt = ' +  $Coma + %trim(PCamR) + $Coma + ' Or ' +                               
              ' Gldcdt = ' +  $Coma + %trim(ICamPa) + $Coma + ' Or ' +                              
              ' Gldcdt >= ' +  $Coma + %trim(PCamR) + $Coma + ')))' +                               
              ' And Gldsct <> ' + '''' + ' ' + '''' +                                               
              ' And Gldcte = 45501979';                                                             
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor1 From :Wsql;                                                                
       Exec Sql                                                                                     
         Declare Cursor1 Scroll Cursor For Cursor1;                                                 
       Exec Sql                                                                                     
         Open Cursor1;                                                                              
       Exec Sql                                                                                     
         fetch Cursor1 Into :WGldcte, :WGldzon, :WGldsct, :WGldtrr;                                 
       Dow sqlcod=*Zeros;                                                                           
           WCedreempla = *Zeros;                                                                    
           WCedAux = *Zeros;                                                                        
                                                                                                    
           Exec Sql                                                                                 
            Select Rsgcgr Into :WCedreempla                                                         
            From Votrea00/Vtmrsgf                                                                   
            Where Rsgcia = :Icia And (Rsgtpo = ' ' Or Rsgtpo = 'P') And                             
                  Rsgcga = :WGldcte And Rsgzng = :Izona And Rsgscg = :WGldsct                       
                  And :W_CampAnt BetWeen Rsgcpi And Rsgcpf                                          
            Order By Rsgcpi Asc                                                                     
            Fetch First 1 Row Only;                                                                 
                                                                                                    
           If WCedreempla > *Zeros;                                                                 
              WCedAux = WGldcte;                                                                    
              WGldcte = WCedreempla;                                                                
           Endif;                                                                                   
                                                                                                    
           Nsuma = *Zeros;                                                                          
           Tvalor = *Zeros;                                                                         
           W_Valor1 = *Zeros;                                                                       
           W_Valor2 = *Zeros;                                                                       
           W_Valor3 = *Zeros;                                                                       
           PrInscReincor();                                                                         
           PrPagosStencil();                                                                        
           PrPagosBonos();                                                                          
           Callp(E) PgmpgrEfec(Icia:PCamR:Izona:OValor);                                            
           If %Len(%Trim(OValor)) > *Zeros;                                                         
              Monitor;                                                                              
                WValor = %Dec(OValor:11:2);                                                         
              On-Error;                                                                             
                Clear WValor;                                                                       
              EndMon;                                                                               
           Else;                                                                                    
              Clear WValor;                                                                         
           Endif;                                                                                   
           WSpginp  = *blanks;                                                                      
           PrGrabar();                                                                              
                                                                                                    
           If WCedreempla > *Zeros;                                                                 
              WCedreempla = *Zeros;                                                                 
              WGldcte = WCedAux;                                                                    
              WCedAux = *Zeros;                                                                     
           Endif;                                                                                   
                                                                                                    
           Exec Sql                                                                                 
           fetch Cursor1 Into :WGldcte, :WGldzon, :WGldsct, :WGldtrr;                               
        EndDo;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor1;                                                                             
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta por Inscripciones o Reincorporaciones      //                        
      //Estrela Efectivas                                                 //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrInscReincor    B                                                                            
     DPrInscReincor    Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(Sum(Iegvpg), 0) ' +                                                    
              'From VotreA00/Vtwiegf ' +                                                            
              'Where Iegcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'IegReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                            
              'IegZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Iegscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                            
              'Iegcdg = ' + %trim(%char(WGldcte))            + ' And ' +                            
              'Iegcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                            
              'IegItp = ''E'' And ' +                                                               
              'IegTpa = ''I''';                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor4 From :Wsql;                                                                
       Exec Sql                                                                                     
         Declare Cursor4 Scroll Cursor For Cursor4;                                                 
       Exec Sql                                                                                     
         Open Cursor4;                                                                              
       Exec Sql                                                                                     
         fetch Cursor4 Into :WValor1;                                                               
                                                                                                    
         W_Valor1 = *Zeros;                                                                         
        If SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         W_Valor1 = WValor1;                                                                        
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor4;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrInscReincor    E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta por Inscripciones o Reincorporaciones      //                        
      //Estrela Efectivas                                                 //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrInscReincorV   B                                                                            
     DPrInscReincorV   Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(Sum(Iegvpg), 0) ' +                                                    
              'From VotreA00/Vtwiegf ' +                                                            
              'Where Iegcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'IegReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                            
              'IegZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Iegscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                            
              'Iegcdg = ' + %trim(%char(WGldcte))            + ' And ' +                            
              'Iegcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                            
              'IegItp = ''E'' And ' +                                                               
              '(IegTpa = ''I''' + ' Or ' + 'IegTpa = ''S''' + ')';                                  
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor4V From :Wsql;                                                               
       Exec Sql                                                                                     
         Declare Cursor4V Scroll Cursor For Cursor4V;                                               
       Exec Sql                                                                                     
         Open Cursor4V;                                                                             
       Exec Sql                                                                                     
         fetch Cursor4V Into :WValor1V;                                                             
                                                                                                    
        W_Valor1V = *Zeros;                                                                         
        If SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         W_Valor1V = WValor1V;                                                                      
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor4V;                                                                            
                                                                                                    
      /End-Free                                                                                     
     PPrInscReincorV   E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta Pagos Stencil Por Cumplimiento de Pedidos  //                        
      //Facturads Segun  su Clasificacion de Alas                         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrPagosStencil   B                                                                            
     DPrPagosStencil   Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DWcodAla          s              4  0  Inz(*Zeros)                                             
      /Free                                                                                         
       WcodAla = *Zeros;                                                                            
       Exec Sql                                                                                     
        Select Cnacod Into :WcodAla                                                                 
        From Votrea00/Vtmcnaf                                                                       
        Where Cnacia = :Icia And                                                                    
              Cnazna = :Izona And                                                                   
              Cnacmp = :PCamR And                                                                   
              Cnasec = :WGldsct And                                                                 
              Cnacdg = :WGldcte                                                                     
        Order By Rrn(Vtmcnaf) Desc                                                                  
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       Wsql = *Blanks;                                                                              
       If WcodAla = *Zeros;                                                                         
          Wsql = 'Select IfNull(Sum(Dggvlr), 0) ' +                                                 
               'From VotreA00/Vttdggf ' +                                                           
               'Where Dggcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
               'DggReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                           
               'DggZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                           
               'Dggscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                           
               'Dggcdg = ' + %trim(%char(WGldcte))            + ' And ' +                           
               'Dggcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                           
               'DggIdp = ''I''';                                                                    
       Else;                                                                                        
          Wsql = 'Select IfNull(Sum(Dggvlr), 0) ' +                                                 
               'From VotreA00/Vttdggf ' +                                                           
               'Where Dggcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
               'DggReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                           
               'DggZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                           
               'Dggscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                           
               'Dggcdg = ' + %trim(%char(WGldcte))            + ' And ' +                           
               'Dggcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                           
               'DggIdp = ''I''' + ' And ' +                                                         
               'Dggcga = ' + %trim(%char(WcodAla));                                                 
       EndIf;                                                                                       
       Exec Sql                                                                                     
         Prepare Cursor2 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor2 Scroll Cursor For Cursor2;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor2;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor2 Into :WValor2;                                                               
                                                                                                    
       W_Valor2 = *Zeros;                                                                           
       If SqlCod <> 100 And SqlCod >= *Zeros;                                                       
          W_Valor2 = WValor2;                                                                       
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor2;                                                                             
      /End-Free                                                                                     
     PPrPagosStencil   E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta Pagos Stencil Por Cumplimiento de Pedidos  //                        
      //Facturads Segun  su Clasificacion de Alas                         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrPagosStencilV  B                                                                            
     DPrPagosStencilV  Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DWcodAla          s              4  0  Inz(*Zeros)                                             
      /Free                                                                                         
       WcodAla = *Zeros;                                                                            
       Exec Sql                                                                                     
        Select Cnacod Into :WcodAla                                                                 
        From Votrea00/Vtmcnaf                                                                       
        Where Cnacia = :Icia And                                                                    
              Cnazna = :Izona And                                                                   
              Cnacmp = :PCamR And                                                                   
              Cnasec = :WGldsct And                                                                 
              Cnacdg = :WGldcte                                                                     
        Order By Rrn(Vtmcnaf) Desc                                                                  
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       Wsql = *Blanks;                                                                              
       If WcodAla = *Zeros;                                                                         
          Wsql = 'Select IfNull(Sum(Dggvlr), 0) ' +                                                 
               'From VotreA00/Vttdggf ' +                                                           
               'Where Dggcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
               'DggReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                           
               'DggZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                           
               'Dggscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                           
               'Dggcdg = ' + %trim(%char(WGldcte))            + ' And ' +                           
               'Dggcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                           
               '(DggIdp = ''I''' + ' Or DggIdp = ''P''' + ')';                                      
       Else;                                                                                        
          Wsql = 'Select IfNull(Sum(Dggvlr), 0) ' +                                                 
               'From VotreA00/Vttdggf ' +                                                           
               'Where Dggcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
               'DggReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                           
               'DggZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                           
               'Dggscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                           
               'Dggcdg = ' + %trim(%char(WGldcte))            + ' And ' +                           
               'Dggcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                           
               '(DggIdp = ''I''' + ' Or DggIdp = ''P''' + ') ' +                                    
               'And Dggcga = ' + %trim(%char(WcodAla));                                             
       EndIf;                                                                                       
       Exec Sql                                                                                     
         Prepare Cursor2V From :Wsql;                                                               
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor2V Scroll Cursor For Cursor2V;                                               
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor2V;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor2V Into :WValor2V;                                                             
                                                                                                    
       W_Valor2V = *Zeros;                                                                          
       If SqlCod <> 100 And SqlCod >= *Zeros;                                                       
          W_Valor2V = WValor2V;                                                                     
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor2V;                                                                            
      /End-Free                                                                                     
     PPrPagosStencilV  E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta Pagos Bonos (Conseguir o Conservar )       //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrPagosBonos     B                                                                            
     DPrPagosBonos     Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(Sum(Pgbvlr), 0) ' +                                                    
              'From VotreA00/VttPgbf ' +                                                            
              'Where Pgbcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'PgbReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                            
              'PgbZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Pgbscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                            
              'Pgbcdg = ' + %trim(%char(WGldcte))            + ' And ' +                            
              'Pgbcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                            
              'PgbIdp = ''I''';                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor3 From :Wsql;                                                                
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor3 Scroll Cursor For Cursor3;                                                 
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor3;                                                                              
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor3 Into :WValor3;                                                               
                                                                                                    
       W_Valor3 = *Zeros;                                                                           
       If SqlCod <> 100 And SqlCod >= *Zeros;                                                       
          W_Valor3 = WValor3;                                                                       
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor3;                                                                             
                                                                                                    
      /End-Free                                                                                     
     PPrPagosBonos     E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta Pagos Bonos (Conseguir o Conservar )       //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrPagosBonosV    B                                                                            
     DPrPagosBonosV    Pi                                                                           
      /Free                                                                                         
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(Sum(Pgbvlr), 0) ' +                                                    
              'From VotreA00/VttPgbf ' +                                                            
              'Where Pgbcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'PgbReg = ' + $Coma + %trim(ORegion)   + $Coma + ' And ' +                            
              'PgbZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Pgbscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                            
              'Pgbcdg = ' + %trim(%char(WGldcte))            + ' And ' +                            
              'Pgbcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                            
              '(PgbIdp = ''I''' + ' Or PgbIdp = ''P''' +                                            
              ' Or PgbIdp = ''E''' + ')';                                                           
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor3V From :Wsql;                                                               
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor3V Scroll Cursor For Cursor3V;                                               
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor3V;                                                                             
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor3V Into :WValor3V;                                                             
                                                                                                    
       W_Valor3V = *Zeros;                                                                          
       If SqlCod <> 100 And SqlCod >= *Zeros;                                                       
          W_Valor3V = WValor3V;                                                                     
       EndIf;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Close cursor3V;                                                                            
                                                                                                    
      /End-Free                                                                                     
     PPrPagosBonosV    E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Graba Registro  en archivo Vtmspgf                  //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrGrabar         B                                                                            
     DPrGrabar         Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DMetodo2          s               n   Inz(*Off)                                                
     DEfectivo2        s               n   Inz(*Off)                                                
     DNvalor           s             11  2 Inz(*Zeros)                                              
     DNro              s              1  0 Inz(1)                                                   
     DIdx              s              1  0 Inz(*Zeros)                                              
     DWcedul           s             10a   Inz(*Blanks)                                             
     DWplan            s             10a   Inz(*Blanks)                                             
     DTsw              s              1a   Inz('N')                                                 
     DTSpgccr          s              3  0 Inz(*Zeros)                                              
     DEvalor           s             11  2 Inz(*Zeros)                                              
      /Free                                                                                         
       Nresta = *Zeros;                                                                             
       IIsw = *Off;                                                                                 
       Nsuma = *Zeros;                                                                              
       Tvalor = *Zeros;                                                                             
       W_Cia = ICia;                                                                                
       W_Zona = IZona;                                                                              
       W_Camp = PCamR;                                                                              
       OvalorW = *Zeros;                                                                            
       PgmMetodo2(W_Cia:ORegion:W_Zona:WGldsct:W_Camp:OvalorW:OvalorG);                             
       Nsuma = *Zeros;                                                                              
       Nsuma = (W_Valor1+W_Valor2+W_Valor3);                                                        
       If OvalorW > *Zeros And Nsuma >= OvalorG;                                                    
          Metodo2 = *On;                                                                            
          Efectivo2 = *Off;                                                                         
          IcedulaSg = *Blanks;                                                                      
          EvalR IcedulaSg = %Char(WGldcte);                                                         
          //Valida Si Campaña tiene excepcion para totalidad de pago efectivo                       
          Chain (Icia:W_Camp) VTMEGMF;                                                              
          If %Found And EGMEST = '' And Nsuma >= EGMVLR;                                            
             Clear IValorSg;                                                                        
          Else;                                                                                     
             Clear IValorSg;                                                                        
             ValorConsignar(Icia:ICedulaSg:IvalorSg);                                               
          EndIf;                                                                                    
      /End-Free                                                                                     
     C                   Z-add     *Zeros        Nvalor                                             
     C                   Movel     IvalorSG      Nvalor                                             
      /Free                                                                                         
          If Nsuma > Nvalor;                                                                        
             If Nvalor > *Zeros;                                                                    
                Nresta = *Zeros;                                                                    
                Nresta = Nsuma - Nvalor;                                                            
                If Nresta > OvalorW;                                                                
                   Efectivo2 = *On;                                                                 
                   IIsw = *On;                                                                      
                   Nro = 2;                                                                         
                Else;                                                                               
                   Efectivo2 = *Off;                                                                
                   Nro = 1;                                                                         
                Endif;                                                                              
             Else;                                                                                  
                If Nvalor <= *Zeros;                                                                
                   Efectivo2 = *On;                                                                 
                   IIsw = *On;                                                                      
                   Nro = 2;                                                                         
                Endif;                                                                              
             Endif;                                                                                 
          Else;                                                                                     
             Efectivo2 = *Off;                                                                      
             Nro = 1;                                                                               
          Endif;                                                                                    
       Else;                                                                                        
          Efectivo2 = *Off;                                                                         
          Nro = 1;                                                                                  
       Endif;                                                                                       
                                                                                                    
       Clear NroC;                                                                                  
       Exec Sql                                                                                     
        Select Count(0) Into :NroC                                                                  
        From Votrea00/Vthzonf                                                                       
        Where Zoncia = :W_Cia And                                                                   
              Zonzon = :W_Zona And                                                                  
              Zonmed = :ICamPa;                                                                     
                                                                                                    
       If NroC > 1;                                                                                 
        Metodo2 = *Off;                                                                             
       Endif;                                                                                       
                                                                                                    
       For Idx = 1 To Nro By 1;                                                                     
           If Metodo2 = *Off;                                                                       
              Tvalor = W_Valor1+W_Valor2+W_Valor3;                                                  
              WSpgccr  = 042;                                                                       
              WSpginp  = ' ';                                                                       
           Else;                                                                                    
              If Nro = 1;                                                                           
                 If Efectivo2 = *Off;                                                               
                    Tvalor = W_Valor1+W_Valor2+W_Valor3;                                            
                    WSpgccr  = 042;                                                                 
                    WSpginp  = ' ';                                                                 
                 Endif;                                                                             
              Endif;                                                                                
              If Nro = 2;                                                                           
                 If Idx = 1;                                                                        
                    If Efectivo2 = *On;                                                             
                       Tvalor = Nvalor;                                                             
                       WSpgccr  = 042;                                                              
                       WSpginp  = ' ';                                                              
                    Endif;                                                                          
                 Endif;                                                                             
                 If Idx = 2;                                                                        
                    If Efectivo2 = *On;                                                             
                       If Nvalor = 0;                    //                                         
                          Nresta = Nsuma - Nvalor;       //                                         
                          //Graba auditoria de pago en efectivo Met2                                
                          Exec Sql                                                                  
                           Insert Into VOTREA00/VTEAPGF Values                                      
                         (:W_Cia, :W_Zona, :W_Camp, :WGldcte, :WGldsct, :Nresta,                    
                             Current_Date, Current_Time, :User);                                    
                       EndIf;                            //                                         
                    Tvalor = Nresta;                                                                
                       If PrVtmipgf() = *On;                                                        
                          WSpgccr  = 043;                                                           
                          WSpginp  = 'T';                                                           
                          IIsw = *On;                                                               
                       Else;                                                                        
                          WSpgccr  = 042;                                                           
                          WSpginp  = ' ';                                                           
                          IIsw = *Off;                                                              
                       Endif;                                                                       
                    Endif;                                                                          
                 Endif;                                                                             
              Endif;                                                                                
           Endif;                                                                                   
           If TValor <> *Zeros;                                                                     
              TSpgccr = *Zeros;                                                                     
              Exec Sql                                                                              
               Select Spgccr Into :TSpgccr                                                          
               From Votrea00/Vtmspgf                                                                
               Where Spgcia = :W_Cia And                                                            
                     Spgzon = :W_Zona And                                                           
                     Spgcam = :W_Camp And                                                           
                     Spgsec = :WGldsct And                                                          
                     Spgtrr = :WGldTrr And                                                          
                     Spgced = :WGldcte And                                                          
                     Spgvds = :Tvalor;                                                              
                                                                                                    
              If TSpgccr > *Zeros;                                                                  
                 WSpgccr = TSpgccr;                                                                 
              Endif;                                                                                
                                                                                                    
            Chain (W_Cia:W_Zona:W_Camp:WGldsct:WGldTrr:WGldcte:WSpgccr) Vtmspgf;                    
              If Not %Found(Vtmspgf);                                                               
                 Tsw = 'S';                                                                         
                 Spgcia  = W_Cia;                                                                   
                 Spgzon  = W_Zona;                                                                  
                 Spgcam  = W_Camp;                                                                  
                 Spgsec  = WGldsct;                                                                 
                 Spgtrr  = WGldTrr;                                                                 
                 Spgced  = WGldcte;                                                                 
                 Spgfds  = %Char(%Date());                                                          
                 Spgvps  = Tvalor;                                                                  
                 Spgnbt  = ' ';                                                                     
                 Spgfbt  = ' ';                                                                     
                 Spgccr  = WSpgccr;                                                                 
                 Spginp  = WSpginp;                                                                 
                 Spgfnp  = ' ';                                                                     
                 Spgvds  = Tvalor;                                                                  
                 PrVtospgf();                                                                       
                                                                                                    
                 PagoG = *Blanks;                                                                   
                 PagoG = %Trim(%Editc(WGldcte:'Z')) + '-' +                                         
                         %Trim(%Editc(W_Valor1:'K')) + 'I1';                                        
                                                                                                    
                 PagoG = *Blanks;                                                                   
                 PagoG = %Trim(%Editc(WGldcte:'Z')) + '-' +                                         
                         %Trim(%Editc(W_Valor2:'K')) + 'I2';                                        
                                                                                                    
                 PagoG = *Blanks;                                                                   
                 PagoG = %Trim(%Editc(WGldcte:'Z')) + '-' +                                         
                         %Trim(%Editc(W_Valor3:'K')) + 'I3';                                        
                                                                                                    
                 PagoG = *Blanks;                                                                   
                 PagoG = %Trim(%Editc(WGldcte:'Z')) + '-' +                                         
                         %Trim(%Editc(Spgvps:'K')) + 'I';                                           
                                                                                                    
                 Write Regspg;                                                                      
                 PrPagosPendientesGuias();                                                          
                 PrVttcpgf();                                                                       
                 PrReteFuente();                                                                    
                 PrActualizaPagoLideres();                                                          
                 PrVtwlpgf();                                                                       
              Else;                                                                                 
             // If Nresta <> *Zeros;                                                                
             //  Tvalor = Nresta;                                                                   
             // Else;                                                                               
                If Nresta = *Zeros;                                                                 
                 Tvalor = Nsuma;                                                                    
                Endif;                                                                              
                 PrVttcpgf();                                                                       
                 TTsw = *Off;                                                                       
                 If Tvalor <> *Zeros;                                                               
                  TTsw = PrValidarPago();                                                           
                 Endif;                                                                             
                 If Spgnbt = *Blanks Or TTsw = *On;                                                 
                    Tsw = 'S';                                                                      
                    Spgfds  = %Char(%Date());                                                       
                    If IIsw = *Off;                                                                 
                       Spgvps  = W_Valor1+W_Valor2+W_Valor3;                                        
                    Else;                                                                           
                     If Tvalor <> 0;                                                                
                      Spgvps  = Tvalor;                                                             
                     Endif;                                                                         
                    Endif;                                                                          
                    If Spgvps > *Zeros;                                                             
                     Clear Spgnbt;                                                                  
                     Clear Spgfbt;                                                                  
                    Endif;                                                                          
                    If TTsw = *On;                                                                  
                     If Kvalor <> *Zeros;                                                           
                      If Kvalor <> Spgvps;                                                          
                       If Tvalor <> 0;                                                              
                        Clear Tvalor1;                                                              
                        Clear Tvalor2;                                                              
                        Tvalor1 =  Kvalor - Tvalor;                                                 
                        Tvalor2 =  Kvalor - Tvalor1;                                                
                        Spgvps = Tvalor2;                                                           
                       Else;                                                                        
                        Spgvps = Kvalor;                                                            
                       Endif;                                                                       
                      Endif;                                                                        
                     Endif;                                                                         
                    Endif;                                                                          
                    Evalor = *Zeros;                                                                
                    Exec Sql                                                                        
                     Select IfNull(Sum(Cpgvlr), 0) Into :Evalor                                     
                     From Votrea00/Vttcpgf                                                          
                     Where Cpgcia = :W_Cia And                                                      
                           Cpgcdg = :WGldcte And                                                    
                           Cpgzon = :W_Zona And                                                     
                           Cpgsec = :WGldsct And                                                    
                           Cpgcmp = :W_Camp And                                                     
                           Cpgcpc = :WSpgccr;                                                       
                    If Evalor > *Zeros;                                                             
                       Spgvds = Evalor;                                                             
                    Endif;                                                                          
                    If TTsw = *On;                                                                  
                       Clear W_sumVds;                                                              
                       w_ccr= 43;                                                                   
                       If Spgccr = 43;                                                              
                          w_ccr = 42;                                                               
                       EndIf;                                                                       
                       Exec Sql                                                                     
                        Select IfNull(Sum(SPGVDS) , 0 ) Into :W_sumVds                              
                        From Votrea00/Vtmspgf                                                       
                        Where Spgcia = :Spgcia And                                                  
                              Spgzon = :Spgzon And                                                  
                              Spgcam = :Spgcam And                                                  
                              Spgsec = :Spgsec And                                                  
                              Spgtrr = :Spgtrr And                                                  
                              Spgced = :Spgced And                                                  
                              Spgccr = :w_ccr;                                                      
                     Spgvds  = W_Valor1v+W_Valor2v+W_Valor3v-W_sumVds;                              
                    Endif;                                                                          
                    PrVtospgf();                                                                    
                    PagoG = *Blanks;                                                                
                    PagoG = %Trim(%Editc(WGldcte:'Z')) + '-' +                                      
                            %Trim(%Editc(Spgvps:'K')) + 'M';                                        
                                                                                                    
                    Update Regspg;                                                                  
                    PrPagosPendientesGuias();                                                       
                    PrReteFuente();                                                                 
                    PrActualizaPagoLideres();                                                       
                    PrVtwlpgf();                                                                    
                 Endif;                                                                             
              EndIf;                                                                                
           Else;                                                                                    
              Tvalor = *Zeros;                                                                      
              Exec Sql                                                                              
               Select Sum(Spgvds) Into :Tvalor                                                      
               From Votrea00/Vtmspgf                                                                
               Where Spgcia = :W_Cia And                                                            
                     Spgzon = :W_Zona And                                                           
                     Spgcam = :W_Camp And                                                           
                     Spgsec = :WGldsct And                                                          
                     Spgtrr = :WGldTrr And                                                          
                     Spgced = :WGldcte;                                                             
              PrPagosPendientesGuias();                                                             
           EndIf;                                                                                   
       EndFor;                                                                                      
       EvalR WGldcteA = %Trim(%Editc(WGldcte:'Z'));                                                 
       WGldcteA = %Xlate(' ':'0':WGldcteA);                                                         
       If Tsw = 'S';                                                                                
          Tsw = 'N';                                                                                
         callp PgmEvaluaPoliticasGuias(W_Cia:W_CampTem:W_Zona:WGldcteA:WGldsct);                    
          callp PgmEvaluaPoliticasComerciales(W_Cia:W_CampTem:W_Camp:W_Zona:                        
                                            WGldcteA:WGldsct);                                      
          callp PgmPoltComer(W_Cia:W_CampTem:W_Zona:WGldcte:WGldsct);                               
          Clear WSpginp;                                                                            
          Exec Sql                                                                                  
          Select Spginp Into :WSpginp                                                               
          From Vtmspgf                                                                              
          Where Spgcia = :W_Cia  and                                                                
                Spgzon = :W_Zona And                                                                
                Spgcam = :W_Camp And                                                                
                Spgsec = :WGldsct And                                                               
                Spgtrr = :WGldTrr And                                                               
                Spgced = :WGldcte;                                                                  
          If WSpginp='E';                                                                           
             qLpgDsa='NO';                                                                          
             Clear QLpgMot;                                                                         
             Exec Sql                                                                               
             Select DNGMOT Into :QLpgMot                                                            
             From Vttdngf                                                                           
             Where DNGCIA = :W_Cia  and                                                             
                    DNGREG = Trim(:ORegion) and                                                     
                    DNGZNG = :W_Zona And                                                            
                    DNGSCG = :WGldsct And                                                           
                    DNGCDG = :WGldcte And                                                           
                    DNGCMP = :W_Camp                                                                
             Order By DNGFEC Desc, DNGHOR desc                                                      
             Fetch First 1 Row Only;                                                                
             If QLpgMot = *Blanks;                                                                  
                Exec Sql                                                                            
                Select LpgMot Into :QLpgMot                                                         
                From Votrea00/Vtwlpgf                                                               
                Where LPGCIA = :W_Cia                                                               
                  And LPGCMP = :W_Camp                                                              
                  And LPGREG = trim(:ORegion)                                                       
                  And LPGZNA = :W_Zona                                                              
                  And LPGSEC = :WGldsct                                                             
                  And LPGCDG = :WGldcte;                                                            
             EndIf;                                                                                 
                                                                                                    
             Exec Sql                                                                               
             Update Votrea00/Vtwlpgf set  LpgMot = :QLpgMot,                                        
                                          LpgDsa = :qLpgDsa,                                        
                                          LpgFDE = ' ',                                             
                                          LpgFDC = ' ',                                             
                                          LpgFVR = ' '                                              
             Where LPGCIA = :W_Cia  and                                                             
                   LPGCMP = :W_Camp And                                                             
                   LPGREG = trim(:ORegion) And                                                      
                   LPGZNA = :W_Zona And                                                             
                   LPGSEC = :WGldsct And                                                            
                   LPGCDG = :WGldcte;                                                               
          EndIf;                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrGrabar         E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Calcula el Valor Total Pagado Guías             //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrValorTotPdo    B                                                                            
     DPrValorTotPdo    Pi            11  2                                                          
      ** Definición de Variables de Trabajo                                                         
     DNvalor0          s             11  2 Inz(*Zeros)                                              
     DNvalor1          s             11  2 Inz(*Zeros)                                              
     DNvalor2          s             11  2 Inz(*Zeros)                                              
      /Free                                                                                         
       // Arch de Trabajo Inscrip o Re-incorpora Rentable                                           
       Exec Sql                                                                                     
        Select IfNull(Sum(Iegvpg), 0) Into :Nvalor0                                                 
        From Votrea00/Vtwiegf                                                                       
        Where Iegcia = :Icia And                                                                    
              Iegzng = :IZona And                                                                   
              Iegscg = :WGldsct And                                                                 
              Iegcdg = :WGldcte And                                                                 
              Iegcmp = :W_Camp And                                                                  
              Iegitp = 'E' And                                                                      
              Iegtpa In ('I', 'P');                                                                 
                                                                                                    
       // Arch de Trabajo Pagos Guías por Stencil con Pedido                                        
       Exec Sql                                                                                     
        Select IfNull(Sum(Dggvlr), 0) Into :Nvalor1                                                 
        From Votrea00/Vttdggf                                                                       
        Where Dggcia = :Icia And                                                                    
              Dggzng = :IZona And                                                                   
              Dggscg = :WGldsct And                                                                 
              Dggcdg = :WGldcte And                                                                 
              Dggcmp = :W_Camp And                                                                  
              Dggidp In ('I', 'P');                                                                 
                                                                                                    
       // Arch de Trabajo Pagos Guías Foco(Conseguir-Conservar)                                     
       Exec Sql                                                                                     
        Select IfNull(Sum(Pgbvlr), 0) Into :Nvalor2                                                 
        From Votrea00/Vttpgbf                                                                       
        Where Pgbcia = :Icia And                                                                    
              Pgbzng = :IZona And                                                                   
              Pgbscg = :WGldsct And                                                                 
              Pgbcdg = :WGldcte And                                                                 
              Pgbcmp = :W_Camp And                                                                  
              Pgbidp In ('I', 'P');                                                                 
                                                                                                    
       Return Nvalor0 + Nvalor1 + Nvalor2;                                                          
                                                                                                    
      /End-Free                                                                                     
     PPrValorTotPdo    E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Graba los Casos Pendientes Pago Guía por Falta //                       
       // del RUT                                                          //                       
       //------------------------------------------------------------------//                       
     PPrPagosPendientesGuias...                                                                     
     P                 B                                                                            
     DPrPagosPendientesGuias...                                                                     
     D                 Pi                                                                           
      ** Definición de Variables de Trabajo                                                         
     DNro              s              3  0  Inz(*Zeros)                                             
     DYnro             s              3  0  Inz(*Zeros)                                             
     DYcia             s              3a   Inz(*Blanks)                                             
     DTced             s             30a   Inz(*Blanks)                                             
     DTcns             s              9a   Inz(*Blanks)                                             
     DTvalor           s             11  2 Inz(*Zeros)                                              
      /Free                                                                                         
       Ycia = W_cia;                                                                                
       Tced = %Trim(%Editc(WGldcte:'Z'));                                                           
       Tcns = *Blanks;                                                                              
       Conversion(Ycia:Tced:Tcns);                                                                  
       Ynro = *Zeros;                                                                               
       Exec Sql                                                                                     
        Select Count(0) Into :Ynro                                                                  
        From Votrea00/Vtmspgf                                                                       
        Where Spgcia = :W_cia And                                                                   
              Spgzon = :W_zona And                                                                  
              Spgcam = :W_camp And                                                                  
              Spgsec = :WGldsct;                                                                    
       If Ynro > *Zeros;                                                                            
          Exec Sql                                                                                  
           Select Count(0) Into :Nro                                                                
           From Votrea00/Vtmrtgf                                                                    
           Where Trim(Char(Rtgidt)) = Trim(:Tced)                                                   
                 And Rtgcia = :W_cia                                                                
                 And (Rtgest = ' ' Or (Rtgest = 'D' And (Rtgcdt =                                   
                 Trim(:PCamR) Or Rtgcdt = Trim(:ICamPa))));                                         
                                                                                                    
          If Nro <= *Zeros;                                                                         
           Pccte = *Blanks;                                                                         
           Pccte = %Trim(%Editc(WGldcte:'Z'));                                                      
           PgmPoliticasRUT(W_cia:W_zona:Pccte                                                       
                           :OrespuestaPgoRUT:OrespuestaIngCnt);                                     
           If OrespuestaPgoRUT = 'S';                                                               
             Clear *Nokey Regppr;                                                                   
             Chain (W_cia:W_camp:W_zona:WGldsct:WGldcte) Vttpprf;                                   
             If not %found(Vttpprf);                                                                
                Pprcia = W_cia;                                                                     
                Pprcmp = W_camp;                                                                    
                Pprzna = W_zona;                                                                    
                Pprsec = WGldsct;                                                                   
                Pprcdg = WGldcte;                                                                   
                                                                                                    
                Tvalor = *Zeros;                                                                    
                Exec Sql                                                                            
                 Select Sum(Spgvps) Into :Tvalor                                                    
                 From Votrea00/Vtmspgf                                                              
                 Where Spgcia = :W_cia And                                                          
                       Spgzon = :W_zona And                                                         
                       Spgcam = :W_camp And                                                         
                       Spgced = :WGldcte And                                                        
                       Spgsec = :WGldsct;                                                           
                                                                                                    
                Pprvlr = Tvalor;                                                                    
                Pprest = 'E';                                                                       
                Pprmot = 'Descuento NO Realizado RUT no Enviado';                                   
                Pprdes = 'Descuento NO Realizado RUT no Enviado';                                   
                                                                                                    
             // Pprdes = 'Pago NO realizado a la guía en C' + %Trim(Spgcam) +                       
             //         ' porque no se tiene la información del RUT.';                              
                Pprfec= %char(%date());                                                             
                Pprhor= %char(%time():*hms);                                                        
                Pprusr= User;                                                                       
                Write Regppr;                                                                       
                //Se agrega la actualizacion al archivo Vtmspgf en el indicador                     
            Chain (W_Cia:W_Zona:W_Camp:WGldsct:WGldTrr:WGldcte:WSpgccr) Vtmspgf;                    
                If %Found(Vtmspgf);                                                                 
                   SPGINP = 'E';                                                                    
                   Update RegSpg;                                                                   
                EndIf;                                                                              
             Else;                                                                                  
                Tvalor = *Zeros;                                                                    
                Exec Sql                                                                            
                 Select Sum(Spgvps) Into :Tvalor                                                    
                 From Votrea00/Vtmspgf                                                              
                 Where Spgcia = :W_cia And                                                          
                       Spgzon = :W_zona And                                                         
                       Spgcam = :W_camp And                                                         
                       Spgced = :WGldcte And                                                        
                       Spgsec = :WGldsct;                                                           
                                                                                                    
                Pprvlr = Tvalor;                                                                    
                Update Regppr;                                                                      
             Endif;                                                                                 
            Endif;                                                                                  
          Endif;                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrPagosPendientesGuias...                                                                     
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Graba Información en el Archivo Vtospgf         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVtospgf        B                                                                            
     DPrVtospgf        Pi                                                                           
      /Free                                                                                         
       Chain (W_Cia:W_Zona:W_Camp:WGldsct:WGldTrr:WGldcte:WSpgccr) Vtospgf;                         
       If Not %Found(Vtospgf);                                                                      
          wSpgcia  = W_Cia;                                                                         
          wSpgzon  = W_Zona;                                                                        
          wSpgcam  = W_Camp;                                                                        
          wSpgfds  = %Char(%Date());                                                                
          wSpgsec  = WGldsct;                                                                       
          wSpgtrr  = WGldTrr;                                                                       
          wSpgced  = WGldcte;                                                                       
          wSpgvps  = Spgvps;                                                                        
          wSpgccr  = WSpgccr;                                                                       
          Write Rgspg;                                                                              
       Else;                                                                                        
          wSpgfds  = %Char(%Date());                                                                
          If wSpgvps <> Spgvps;                                                                     
             wSpgvps = Spgvps;                                                                      
          Endif;                                                                                    
          Update Rgspg;                                                                             
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrVtospgf        E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si la Guía Tiene Cuenta Activa        //                       
       //------------------------------------------------------------------//                       
     PPrVtmipgf        B                                                                            
     DPrVtmipgf        Pi              n                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DPsw              s               n   Inz(*Off)                                                
     DKnro             s              4  0 Inz(*Zeros)                                              
     DYcia             s              3a   Inz(*Blanks)                                             
     DTced             s             30a   Inz(*Blanks)                                             
     DTcns             s              9a   Inz(*Blanks)                                             
      /Free                                                                                         
       Ycia = Icia;                                                                                 
       Tced = *Blanks;                                                                              
       Tcns = %Trim(%Editc(WGldcte:'Z'));                                                           
       Conversion(Ycia:Tced:Tcns);                                                                  
       Exec Sql                                                                                     
        Select Count(0) Into :Knro                                                                  
        From Votrea00/Vtmipgf                                                                       
        Where Ipgcia = :Icia And                                                                    
              (Trim(Char(Ipgcgc)) = Trim(:Tced) Or                                                  
               Trim(Char(Ipgcgc)) = Trim(:Tcns))                                                    
               And IpgEst = ' ';                                                                    
                                                                                                    
       If Knro > *Zeros;                                                                            
          Psw = *On;                                                                                
       Endif;                                                                                       
                                                                                                    
       Return Psw;                                                                                  
      /End-Free                                                                                     
     PPrVtmipgf        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Graba Información en el Archivo Vttcpgf         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVttcpgf        B                                                                            
     DPrVttcpgf        Pi                                                                           
     ** Definiciòn de Variables de Trabajo                                                          
     DOfecha           s             10a   Inz(*Blanks)                                             
     DTnro             s              2  0 Inz(*Zeros)                                              
     DKvalor           s             11  2 Inz(*Zeros)                                              
     DKvalorf          s             11  2 Inz(*Zeros)                                              
     DKvalorf1         s             11  2 Inz(*Zeros)                                              
     DJnro             s              2  0 Inz(*Zeros)                                              
     DHvalor           s             11  2 Inz(*Zeros)                                              
      /Free                                                                                         
       Clear Kvalor;                                                                                
       Exec Sql                                                                                     
        Select IfNull(Sum(Cpgvlr), 0) Into :Kvalor                                                  
        From Votrea00/Vttcpgf                                                                       
        Where Cpgcia = :W_Cia And                                                                   
              Cpgzon = :W_Zona And                                                                  
              Cpgsec = :WGldsct And                                                                 
              Cpgcdg = :WGldcte And                                                                 
              Cpgcmp = :W_Camp;                                                                     
                                                                                                    
       Clear Jnro;                                                                                  
       Exec Sql                                                                                     
        Select Count(0) Into :Jnro                                                                  
        From Votrea00/Vthzonf                                                                       
        Where Zoncia = :W_Cia And                                                                   
              Zonzon = :W_Zona And                                                                  
              Zonmed = :ICamPa;                                                                     
                                                                                                    
       Hvalor = Tvalor;                                                                             
       If Jnro > 1;                                                                                 
        PrInscReincorT();                                                                           
        PrPagosStencilT();                                                                          
        PrPagosBonosT();                                                                            
        Clear KvalorF;                                                                              
        Clear KvalorF1;                                                                             
        KvalorF = W_Valor1T+W_Valor2T+W_Valor3T;                                                    
        Kvalorf1 = KvalorF - Kvalor;                                                                
                                                                                                    
       If Kvalorf1 > 0;                                                                             
       Clear Hvalor;                                                                                
       Hvalor = Kvalorf1;                                                                           
       Ofecha = %Char(%Date());                                                                     
       Chain (W_Cia:W_Zona:WGldsct:WGldcte:W_Camp:Hvalor:Ofecha:Spgccr) Vttcpgf;                    
       If Not %Found(Vttcpgf);                                                                      
          Cpgcia = W_Cia;                                                                           
          Cpgzon = W_Zona;                                                                          
          Cpgsec = WGldsct;                                                                         
          Cpgcdg = WGldcte;                                                                         
          Cpgcmp = W_Camp;                                                                          
          Cpgvlr = Hvalor;                                                                          
          Cpgfec = Ofecha;                                                                          
          Cpghor = %Char(%Time());                                                                  
          Cpgusr = User;                                                                            
          Cpgcpc = Spgccr;                                                                          
          Write Regcpg;                                                                             
        Endif;                                                                                      
        Endif;                                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPrVttcpgf        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Graba Información en el Archivo Vtwlpgf         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrVtwlpgf        B                                                                            
     DPrVtwlpgf        Pi                                                                           
     ** Definiciòn de Variables de Trabajo                                                          
     Dw_nombreLider    s             30A                                                            
     DwindZonaTpoPlan  s             20A                                                            
     DwdescripContabl  s            100A                                                            
     DwindicpagoEfect  s              2A                                                            
      /Free                                                                                         
       If QLpgVde = QLpgVdc;                                                                        
          Clear qLpgVdc;                                                                            
       EndIf;                                                                                       
       If QLpgVde < *Zeros;                                                                         
          QLpgVde = QLpgVde*-1;                                                                     
       Elseif QLpgVde = *Zeros;                                                                     
          clear qLpgFde;                                                                            
       EndIf;                                                                                       
       If QLpgVdc < *Zeros;                                                                         
          QLpgVdc = QLpgVdc *-1;                                                                    
       ElseIf QLpgVdc = *Zeros;                                                                     
          Clear QLpgFdc;                                                                            
       EndIf;                                                                                       
       QLpgTds= qLPGVDE + qLPGVDC;                                                                  
       QLpgCia= W_Cia;                                                                              
       QLpgCmp= W_Camp;                                                                             
       QLpgReg= ORegion;                                                                            
       QLpgZna= W_Zona;                                                                             
       QLpgSec= WGldsct;                                                                            
       QLpgCdg= WGldcte;                                                                            
       Exec Sql                                                                                     
         Select MLNAME Into :qLpgNom                                                                
         From LMLLST01                                                                              
         Where MLCSTÑ = :QLpgCdg;                                                                   
       QLpgMot= Pprmot;                                                                             
       Clear YSpgfbt;                                                                               
       Exec Sql                                                                                     
        Select Spginp, Spgfbt Into :WSpginp, :YSpgfbt                                               
        From Vtmspgf                                                                                
        Where Spgcia = :W_Cia  and                                                                  
              Spgzon = :W_Zona And                                                                  
              Spgcam = :W_Camp And                                                                  
              Spgsec = :WGldsct And                                                                 
              Spgtrr = :WGldTrr And                                                                 
              Spgced = :WGldcte And                                                                 
              SPGCCR = :Spgccr;                                                                     
       If WSpginp='E';                                                                              
          qLpgDsa='NO';                                                                             
       Else;                                                                                        
          qLpgDsa='SI';                                                                             
          Clear QLpgMot;                                                                            
       EndIf;                                                                                       
       Clear DsNewPlanGu;                                                                           
       PgmNewPlanG(QLPgCia:QLPgZna:Icaso:Icedula:Ifecha:Orespuesta);                                
       If Orespuesta = 'S';                                                                         
          QLpgIpt= 'S';                                                                             
       Else;                                                                                        
          QLpgIpt= 'N';                                                                             
       Endif;                                                                                       
       Clear fields1;                                                                               
       Clear ycpgfbte;                                                                              
       Clear ycpgfbtc;                                                                              
       Exec Sql                                                                                     
       Select Lpgvde, Lpgvdc, Lpgfde, Lpgfdc                                                        
             into :Ylpgvde,:Ylpgvdc,:YLpgfde,:YLpgfdc                                               
       From Votrea00/Vtwlpgf                                                                        
       Where LPgCia= :QLPgCia And                                                                   
             LPgCmp= :QLPgCmp And                                                                   
             LPgReg= :QLPgReg And                                                                   
             LPgZna= :QLPgZna And                                                                   
             LPgSec= :QLPgSec And                                                                   
             LPgCdg= :QLPgCdg And                                                                   
             LpgIpt= :QLpgIpt;                                                                      
       SqlCodAux = Sqlcod;                                                                          
       If Spgccr = 43;// pago efectivo                                                              
          Qlpgvde = Qlpgvde;                                                                        
          Qlpgfde = YSpgfbt;                                                                        
          Clear TnroP;                                                                              
          Exec Sql                                                                                  
           Select Count(0) Into :TnroP                                                              
           From Votrea00/Vtmspgf                                                                    
           Where SpgCia= :QLPgCia And                                                               
                 Spgcam= :QLPgCmp And                                                               
                 Spgzon= :QLPgZna And                                                               
                 Spgsec= :QLPgSec And                                                               
                 SpgCed= :QLPgCdg And                                                               
                 Spgccr = 42;                                                                       
          If TnroP > 0;                                                                             
           Qlpgvdc = YLpgvdc;                                                                       
           Qlpgfdc = YLpgfdc;                                                                       
          Endif;                                                                                    
       EndIf;                                                                                       
       If Spgccr = 42; // pago CArtera                                                              
          Qlpgvdc = Qlpgvdc;                                                                        
          Qlpgfdc = Yspgfbt;                                                                        
          Clear TnroP;                                                                              
          Exec Sql                                                                                  
           Select Count(0) Into :TnroP                                                              
           From Votrea00/Vtmspgf                                                                    
           Where SpgCia= :QLPgCia And                                                               
                 Spgcam= :QLPgCmp And                                                               
                 Spgzon= :QLPgZna And                                                               
                 Spgsec= :QLPgSec And                                                               
                 SpgCed= :QLPgCdg And                                                               
                 Spgccr = 43;                                                                       
          If TnroP > 0;                                                                             
           Qlpgvde = Ylpgvde;                                                                       
           Qlpgfde = Ylpgfde;                                                                       
          Endif;                                                                                    
       EndIf;                                                                                       
       If Qlpgvde = qlpgvdc;                                                                        
          Clear qlpgvdc;                                                                            
          Clear qlpgfdc;                                                                            
       EndIf;                                                                                       
       Qlpgtds= Qlpgvde + Qlpgvdc;                                                                  
       If SqlcodAux = *Zeros;                                                                       
          Exec Sql                                                                                  
            Update Votrea00/Vtwlpgf set LpgTds = :QLpgTds,                                          
                                        LpgVde = :QLpgVde,                                          
                                        LpgVdc = :QLpgVdc,                                          
                                        LpgFde = :QLpgFde,                                          
                                        LpgFdc = :QLpgFdc,                                          
                                        LpgMot = :QLpgMot,                                          
                                        LpgDsa = :qLpgDsa                                           
            Where LPgCia= :QLPgCia And                                                              
                  LPgCmp= :QLPgCmp And                                                              
                  LpgReg= :QLpgReg And                                                              
                  LPgZna= :QLPgZna And                                                              
                  LPgSec= :QLPgSec And                                                              
                  LPgCdg= :QLPgCdg And                                                              
                  LpgIpt= :QLpgIpt;                                                                 
       Else;                                                                                        
          Exec Sql                                                                                  
          Insert into Votrea00/Vtwlpgf values(:Fields);                                             
       EndIf;                                                                                       
       Clear Fields;                                                                                
      /End-Free                                                                                     
     PPrVtwlpgf        E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo: Pr_Cal_Retenciones                                     //                        
      //           Realiza la verificación por pago a lider acumulando el //                        
      //           valor, para verificar si aplica para retención en la   //                        
      //           Fuente                                                 //                        
      //////////////////////////////////////////////////////////////////////                        
     PPr_Cal_Retenciones...                                                                         
     P                 B                                                                            
     DPr_Cal_Retenciones...                                                                         
     D                 Pi                                                                           
     Dy_Nsuma                        11  2                                                          
     ** Definiciòn de Variables de Trabajo                                                          
     Da_Nsuma          s             11  2 Inz(*Zeros)                                              
     Dw_VlrRetencion   s             11  2 Inz(*Zeros)                                              
     Dw_Contar         s              5  0 Inz(*Zeros)                                              
     Dw_CountExiste    s              9  0 Inz(*Zeros)                                              
     Da_NsumaTemp      s             11  2 Inz(*Zeros)                                              
     DWttlret          s             15  2 Inz(*Zeros)                                              
     DFechass          s               d                                                            
      /Free                                                                                         
       Clear w_VlrALiquidado;                                                                       
       Clear W_PorcentaRetencion;                                                                   
       Clear w_SumVlrRet;                                                                           
       Clear w_SumVlrLq;                                                                            
       // Prototipo que realiza registro en el archivo de retención en caso de q                    
       // no se hubiera sido posteado sin haber calculado retención                                 
       Pr_RegistraPosteosAnterior();                                                                
       Flag = 0;                                                                                    
       // Verifica si exite informacion de lider para campaña                                       
       Exec Sql                                                                                     
       Select Ifnull(Sum(CPGVLL) , 0 ) Into :WTtlRet                                                
       From   Votrea00/Vtwcpgf                                                                      
        Where CPGCIA = :W_Cia                                                                       
          And CPGCED = :WGldcte                                                                     
          And CPGZNA = :W_Zona                                                                      
          And CPGCMP = :W_Camp;                                                                     
       // Si valor de pago lider es mayor a total en log de retefuente                              
       // aplica para verificacion de descuento.                                                    
       w_VlrALiquidado = y_Nsuma - WTtlRet;                                                         
       If w_VlrALiquidado > *Zeros;                                                                 
          //VerifiPr_ValidaRangoRetenera dentro del parametrizado a Retener                         
          Pr_ValidaRangoRetener(w_VlrALiquidado:W_PorcentaRetencion);                               
         // Combierte porcentaje 0.00%                                                              
         If Vrgest = ' ';                                                                           
          W_PorcentaRetencion *= 0.01;                                                              
         Endif;                                                                                     
                                                                                                    
          w_VlrRetencion = W_PorcentaRetencion * w_VlrALiquidado ;                                  
          If w_VlrRetencion > *Zeros;                                                               
             a_Nsuma=w_VlrALiquidado-w_VlrRetencion;                                                
             If a_Nsuma < *Zeros;                                                                   
              a_NsumaTemp = a_Nsuma * (-1);                                                         
              Exec Sql                                                                              
              Insert Into Votrea00/Vtwsrff Values(:W_Cia,:WGldcte,:W_Zona,                          
                         :WGldsct,:W_Camp,:w_VlrRetencion,:a_NsumaTemp,                             
                         USER,Current Date  , Current_time );                                       
             Endif;                                                                                 
          EndIf;                                                                                    
          Clear Fields2;                                                                            
          // Recupera el nombre del LMLLST01                                                        
          Exec Sql                                                                                  
          Select MLNAME into :q_CPGNOM                                                              
          from LMLLST01                                                                             
          where MLCSTÑ = :WGldcte;                                                                  
          Clear w_TemCpgspr;                                                                        
          CLear w_SaldoPen;                                                                         
          CLear w_SumVlrLq;                                                                         
          CLear w_SumVlrRet;                                                                        
                                                                                                    
          // Recupera Valor pendiente a retener                                                     
          Pr_Cal_RetencionesVlrNegativo(a_Nsuma:w_VlrALiquidado);                                   
          If q_CPGSPR > *Zeros;                                                                     
             w_SaldoPen      = q_CPGSPR;                                                            
             w_SumVlrLq      = w_VlrALiquidado;                                                     
             w_SumVlrRet     = w_VlrALiquidado;                                                     
             Clear wx_Vrfipc;                                                                       
             Pr_InsertaRetencionVtwvrff();                                                          
          ElseIf w_TemCpgspr > *Zeros;                                                              
             w_SumVlrLq      = w_VlrALiquidado;                                                     
             w_SumVlrRet     = w_TemCpgspr;                                                         
             Clear wx_Vrfipc;                                                                       
             Pr_InsertaRetencionVtwvrff();                                                          
          EndIf;                                                                                    
          Clear w_TemCpgspr;                                                                        
          CLear w_SaldoPen;                                                                         
          CLear w_SumVlrLq;                                                                         
          CLear w_SumVlrRet;                                                                        
          // Verifica el consepto por el que se esta realizando el pago                             
          // 43 pago efectivo, 42 pago cartera                                                      
          If Spgccr = 43;                                                                           
             q_CPGVAE = a_Nsuma;                                                                    
          Else;                                                                                     
             Q_CPGVPC = a_Nsuma;                                                                    
          EndIf;                                                                                    
          Q_cpgvds = a_Nsuma;                                                                       
          QSpgfds = %Char(%Date());                                                                 
          Exec Sql                                                                                  
          Insert Into   Votrea00/vtwCpgf Values(:W_Cia,:WGldcte  ,:w_Zona ,                         
                 :WGldsct,:w_camp ,:w_VlrALiquidado,:Q_cpgvds,:w_VlrRetencion,                      
                 User , Current Date ,Current_time, :q_CPGNOM ,                                     
                 :q_CPGSPR,:Q_cpgvds ,:q_CPGVPC,:q_CPGVAE,:q_CPGIPE,:q_CPGIPP);                     
          If w_VlrRetencion > *Zeros;                                                               
             Q_cpgvds   =w_VlrALiquidado-q_CPGSPR;                                                  
          EndIf;                                                                                    
                                                                                                    
          // Recupera Valor de pagos y valor retenido para verificar si cumple.                     
          Exec Sql                                                                                  
           Select Sum(CPGVLL), Sum(CPGRTE) into :w_SumVlrLq,:w_SumVlrRet                            
           From   Votrea00/Vtwcpgf                                                                  
           Where CPGCIA = :W_Cia                                                                    
             And CPGCED = :WGldcte                                                                  
             And CPGZNA = :W_Zona                                                                   
             And SUBSTR(char(CPGfec, iso) , 1 , 7) =                                                
                 SubString(Char(Current Date, Iso), 1, 7);                                          
                                                                                                    
          //Verifica si Pago se ecuentra dentro del parametrizado a Retener                         
          Pr_ValidaRangoRetener(w_SumVlrLq:W_PorcentaRetencion);                                    
          If W_PorcentaRetencion > *Zeros;                                                          
             W_PorcentaRetencion = W_PorcentaRetencion * 0.01;                                      
             w_VlrRetencion = W_PorcentaRetencion * w_SumVlrLq ;                                    
             IF %Dech(w_VlrRetencion:11:0) <> %Dech(w_SumVlrRet:11:0);                              
                If a_Nsuma = *Zeros Or W_salretpen > *Zeros or Flag = 1;                            
                   w_VlrRetencion = w_VlrRetencion - w_SumVlrRet;                                   
                EndIf;                                                                              
                If w_VlrRetencion > *Zeros;                                                         
                   a_Nsuma= w_VlrALiquidado - w_VlrRetencion;                                       
                   If a_Nsuma < *Zeros;                                                             
                      a_NsumaTemp = a_Nsuma * (-1);                                                 
                      Exec Sql                                                                      
                      Insert Into Votrea00/Vtwsrff Values(:W_Cia,:WGldcte,                          
                                  :W_Zona,:WGldsct,:W_Camp,:w_VlrRetencion,                         
                                  :a_NsumaTemp, USER, :QSpgfds , Current_time );                    
                   Endif;                                                                           
                   If a_Nsuma < *Zeros;                                                             
                      a_NsumaTemp *= (-1);                                                          
                      Exec Sql                                                                      
                      Update Votrea00/Vtwsrff Set Srfrte = :w_VlrRetencion,                         
                                                  Srfsdp = :a_NsumaTemp                             
                      Where Rrn(Vtwsrff) = (Select Rrn(Vtwsrff)                                     
                                            From Votrea00/Vtwsrff                                   
                                             Where Srfcia =:W_Cia   And                             
                                                   Srfced =:WGldcte And                             
                                                   Srfzna =:W_Zona  And                             
                                                   Srfsec =:WGldsct                                 
                                              Order By Rrn(Vtwsrff) Desc                            
                                              Fetch First 1 Row Only) ;                             
                   Endif;                                                                           
                                                                                                    
                   If Q_cpgvds= *Zeros;                                                             
                      Clear a_Nsuma;                                                                
                   EndIf;                                                                           
                   Clear w_vlrttl;                                                                  
                   w_vlrttl =Q_cpgvds;                                                              
                   Pr_Cal_RetencionesVlrNegativo(a_Nsuma:w_vlrttl);                                 
                   If w_vlrttl = *Zeros;                                                            
                      q_CPGSPR += w_VlrRetencion;                                                   
                   ElseIf w_vlrttl > *Zeros and q_CPGSPR = *Zeros                                   
                      And a_Nsuma  > *Zeros;                                                        
                      a_Nsuma = w_vlrttl - w_VlrRetencion;                                          
                   EndIf;                                                                           
                   Clear q_CPGVAE;                                                                  
                   Clear Q_CPGVPC;                                                                  
                   If Spgccr = 43;                                                                  
                      q_CPGVAE = a_Nsuma;                                                           
                   Else;                                                                            
                      Q_CPGVPC = a_Nsuma;                                                           
                   EndIf;                                                                           
                   Exec Sql                                                                         
                   Update    Votrea00/Vtwcpgf SET CPGVLL = :w_VlrALiquidado,                        
                           CPGVDS = :a_Nsuma, CPGRTE = :w_VlrRetencion,                             
                           CPGSPR = :q_CPGSPR , CPGVRP = :a_Nsuma  ,                                
                           CPGUSR = USER    , CPGFEC = Current Date ,                               
                           CPGHOR = Current_Time ,                                                  
                           CPGVPC = :Q_CPGVPC,                                                      
                           CPGVAE = :q_CPGVAE                                                       
                   Where RRN(vtwCpgf) = (SELECT RRN(vtwCpgf)                                        
                                         From   Votrea00/Vtwcpgf                                    
                                         Where CPGCIA = :W_Cia                                      
                                           And CPGCED = :WGldcte                                    
                                           And CPGZNA = :W_Zona                                     
                                           Order By RRN(vtwCpgf) Desc                               
                                           Fetch First 1 Row Only) ;                                
                EndIf;                                                                              
             EndIf;                                                                                 
          EndIf;                                                                                    
          // Si pago aplica para descuento de retefuente se inserta valor a VTWV                    
          // el descuento se realiza en el posteo a cartera                                         
          If w_VlrRetencion > *Zeros;                                                               
             Clear w_VlrDescu;                                                                      
             Clear w_SumVlrRet;                                                                     
             Clear w_SumVlrLq;                                                                      
             Clear w_SaldoPen;                                                                      
             // Recupera Valor de pagos y valor retenido para verificar si cumpl                    
             Exec Sql                                                                               
             Select Sum(CPGVLL),Sum(CPGVDS), Sum(CPGRTE), Sum(CPGSPR)                               
             into :w_SumVlrLq,:w_VlrDescu,:w_SumVlrRet,:w_SaldoPen                                  
              From   Votrea00/Vtwcpgf                                                               
              Where CPGCIA = :W_Cia                                                                 
                And CPGCED = :WGldcte                                                               
                And CPGZNA = :W_Zona                                                                
                And SUBSTR(char(CPGfec, iso) , 1 , 7) =                                             
                    SubString(Char(Current Date, Iso), 1, 7);                                       
             Clear w_VlrPosteDes;                                                                   
             Exec Sql                                                                               
             Select  IfNull(Sum(VRFVRR) , 0 ) into :w_VlrPosteDes                                   
             From   Votrea00/vtwvrff                                                                
             Where Vrfcia = :W_Cia                                                                  
               And Vrfzna = :W_Zona                                                                 
               And Vrfced = :WGldcte                                                                
               And (Vrfipc = ' ' Or Trim(Vrfipc) = 'P')                                             
               And Vrfprr <> 999.90                                                                 
               And SUBSTR(char(vrffec) , 1 , 7) =                                                   
                SubString(Char(current timestamp ), 1, 7);                                          
             w_AcuDtl = w_SumVlrLq - (w_VlrDescu + w_VlrPosteDes);                                  
             If w_AcuDtl > *Zeros;                                                                  
                Exec Sql                                                                            
                Select CPGVLL , CPGRTE , CPGSPR                                                     
                into :w_SumVlrLq,:w_SumVlrRet,:w_SaldoPen                                           
                From   Votrea00/Vtwcpgf                                                             
                Where Cpgcia = :W_Cia                                                               
                  And Cpgced = :WGldcte                                                             
                  And CpgCmp = :W_Camp                                                              
                  And Cpgzna = :W_Zona                                                              
                  Order By RRN(vtwCpgf) Desc                                                        
                  Fetch First 1 Row Only;                                                           
                w_SumVlrRet = w_AcuDtl;                                                             
                w_VlrALiquidado =w_SumVlrLq;                                                        
                Clear wx_Vrfipc;                                                                    
                Pr_InsertaRetencionVtwvrff();                                                       
             EndIf;                                                                                 
          EndIf;                                                                                    
          PrDescuentoSaldoPendiente(w_TtlPagar);                                                    
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPr_Cal_Retenciones...                                                                         
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Graba Información en el Archivo Vtwlpgf         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPr_Cal_RetencionesVlrNegativo...                                                              
     P                 B                                                                            
     DPr_Cal_RetencionesVlrNegativo...                                                              
     D                 Pi                                                                           
     Dw_resultPagoCal                11  2                                                          
     Dw_vlrPagosIn                   11  2                                                          
     Dw_SldPendRtecin  s             11  2                                                          
     Dw_SaldoPendiete  s             11  2                                                          
      /Free                                                                                         
       Clear W_salretpen;                                                                           
       Exec Sql                                                                                     
       Select sum(CPGSPR) into :q_CPGSPR                                                            
       From   Votrea00/vtwCpgf                                                                      
       where CPGCED =:WGldcte;                                                                      
       w_TemCpgspr = q_CPGSPR ;                                                                     
       W_salretpen = q_CPGSPR ;                                                                     
       If w_resultPagoCal <> *Zeros;                                                                
          q_CPGSPR         = w_resultPagoCal - q_CPGSPR ;                                           
          If q_CPGSPR < *Zeros;                                                                     
             q_CPGSPR = q_CPGSPR * (-1) ;                                                           
             q_CPGVRP = *Zeros;                                                                     
             Clear w_resultPagoCal;                                                                 
             Clear W_salretpen;                                                                     
          ELse;                                                                                     
             w_resultPagoCal = q_CPGSPR;                                                            
             Clear q_CPGSPR;                                                                        
          EndIf;                                                                                    
          Exsr SrUpdate;                                                                            
       ElseIf w_vlrPagosIn > *Zeros;// And q_CPGSPR > *Zeros;                                       
          q_CPGSPR = w_vlrPagosIn - q_CPGSPR;                                                       
          If q_CPGSPR < *Zeros;                                                                     
             q_CPGSPR = q_CPGSPR     * -1;                                                          
             Clear w_resultPagoCal;                                                                 
             Clear W_salretpen;                                                                     
          ELse;                                                                                     
             Flag = 1;                                                                              
             w_resultPagoCal = q_CPGSPR;                                                            
             Clear q_CPGSPR;                                                                        
             Clear W_salretpen;                                                                     
          EndIf;                                                                                    
          Exsr SrUpdate;                                                                            
       EndIf;                                                                                       
       BegSr SrUpdate;                                                                              
          Exec Sql                                                                                  
          Update   Votrea00/vtwCpgf SET                                                             
                                       CPGSPR = 0                                                   
          Where RRN(vtwCpgf) = (SELECT RRN(vtwCpgf)                                                 
                               fROM   Votrea00/vtwCpgf                                              
                               Where CPGCIA = :W_Cia                                                
                                 And CPGCED = :WGldcte                                              
                                 And CPGZNA = :W_Zona                                               
                                 Order By RRN(vtwCpgf) Desc                                         
                                 Fetch First 1 Row Only) ;                                          
       ENdSr;                                                                                       
      /End-Free                                                                                     
     PPr_Cal_RetencionesVlrNegativo...                                                              
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento: PrDescuentoSaldoPendiente                          //                        
      //               realiza descuento de valores pendientes por cobrar //                        
      //               se verifica en archivo VTWVRTF                     //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrDescuentoSaldoPendiente...                                                                  
     P                 B                                                                            
     DPrDescuentoSaldoPendiente...                                                                  
     D                 Pi                                                                           
     DI_Nsuma                        11  2                                                          
     Dw_cobrotemp      s             15  2  Inz(*Zeros)                                             
     dW_saldoCobrar    S             15  2 Inz(*Zeros)                                              
     dW_CobradoXDeu    S             15  2 Inz(*Zeros)                                              
      *Recupera Valor que se ha cobrado por deuda antes de posteo                                   
      /Free                                                                                         
       Clear w_SaldoPen;                                                                            
       Clear w_SumVlrLq;                                                                            
       Clear w_SumVlrRet;                                                                           
       Clear W_saldoCobrar;                                                                         
       Clear w_VlrPosteDes;                                                                         
       // Recupera Valor Deuda antigua para realizar descuento                                      
       Exec Sql                                                                                     
       Select IfNull(Max(VrtVlr) , 0 )-IfNull(Sum(VRFVRR) , 0 ),                                    
       IfNull(Sum(VRFVRR) , 0 )                                                                     
       Into :W_saldoCobrar,:W_CobradoXDeu                                                           
       From   Votrea00/vtwvrtf                                                                      
       Left join   Votrea00/Vtwvrff on Vrfcia=VRTCIA And Vrfced=VRTCED                              
       And Vrfipc='D' And Vrfzna =:W_Zona                                                           
       Where Vrtcia =:W_Cia                                                                         
         And Vrtced =:WGldcte;                                                                      
                                                                                                    
       If W_saldoCobrar > *Zeros;                                                                   
          Clear w_VlrDescu;                                                                         
          Clear w_SumVlrRet;                                                                        
          Clear w_SumVlrLq;                                                                         
          Clear w_SaldoPen;                                                                         
          Clear w_AcuDtl;                                                                           
          Exec Sql                                                                                  
          Select CPGVDS                                                                             
          into  :w_AcuDtl                                                                           
          From   Votrea00/Vtwcpgf                                                                   
          Where Cpgcia = :W_Cia And Cpgced = :WGldcte And CpgCmp = :W_Camp                          
            And Cpgzna = :W_Zona                                                                    
            Order By RRN(vtwCpgf) Desc                                                              
            Fetch First 1 Row Only;                                                                 
          w_cobrotemp = w_AcuDtl-W_saldoCobrar;                                                     
          If w_cobrotemp >= *Zeros;                                                                 
             w_AcuDtl =  W_saldoCobrar;                                                             
          EndIf;                                                                                    
          If w_AcuDtl > *Zeros;                                                                     
             w_SumVlrRet = w_AcuDtl;                                                                
             w_VlrALiquidado = w_AcuDtl;                                                            
             Clear W_PorcentaRetencion;                                                             
             W_PorcentaRetencion = 9.999;                                                           
             Clear wx_Vrfipc;                                                                       
             wx_Vrfipc='D';                                                                         
             Pr_InsertaRetencionVtwvrff();                                                          
          EndIf;                                                                                    
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrDescuentoSaldoPendiente...                                                                  
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Graba Información en el Archivo Vtwlpgf         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrRecuperaPosteo...                                                                           
     P                 B                                                                            
     DPrRecuperaPosteo...                                                                           
     D                 Pi                                                                           
     Dw_campana        s              4A                                                            
      /Free                                                                                         
       //Verifica si Zona es parte de nuevo plan de Guías                                           
       w_campana=W_Camp;                                                                            
       Clear DsNewPlanGu;                                                                           
       Icedula = %trim(%editc(WGldcte:'P'));                                                        
       PgmNewPlanG(W_Cia:W_Zona:Icaso:Icedula:Ifecha:Orespuesta);                                   
       If Orespuesta = 'S';                                                                         
          w_campana = W_CampTem;                                                                    
       Endif;                                                                                       
       Clear V_Valtra;                                                                              
       Clear w_AcuDtl;                                                                              
       Exec sql                                                                                     
       Declare csr3 cursor for                                                                      
        Select *                                                                                    
        from pardtl                                                                                 
        Where ADCSTÑ = :WGldcte;                                                                    
          Exec Sql                                                                                  
            Open csr3;                                                                              
          Exec Sql                                                                                  
            fetch csr3 Into :Fields3;                                                               
       Dow SqlCod = *Zeros;                                                                         
           Clear WADTNDÑ;                                                                           
           WADTNDÑ  = qADTNDÑ;                                                                      
           CAMBIARCq(WADTNDÑ);                                                                      
           Clear w_WADTNDÑ;                                                                         
           w_WADTNDÑ = WADTNDÑ;                                                                     
           IF w_WADTNDÑ2 = %Trim(w_campana);                                                        
              If QADTRAM < *Zeros;                                                                  
                 QADTRAM = QADTRAM * -1;                                                            
                 w_contatrandtl += 1;                                                               
                 V_Valtra(w_contatrandtl) = QADTRAM;                                                
                 w_AcuDtl  += QADTRAM;                                                              
              EndIf;                                                                                
           EndIf;                                                                                   
          Exec Sql                                                                                  
            fetch csr3 Into :Fields3;                                                               
       EndDO;                                                                                       
          Exec Sql                                                                                  
            Close csr3;                                                                             
      /End-Free                                                                                     
     PPrRecuperaPosteo...                                                                           
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo Registra Pagos de poesteo Anteriores                   //                         
      //////////////////////////////////////////////////////////////////////                        
     PPr_RegistraPosteosAnterior...                                                                 
     P                 B                                                                            
     DPr_RegistraPosteosAnterior...                                                                 
     D                 Pi                                                                           
     DW_ExisteR        s              9  0                                                          
      /Free                                                                                         
       Clear W_ExisteR;                                                                             
       Exec Sql                                                                                     
       Select Count(0) into :W_ExisteR                                                              
       From Votrea00/Vtwcpgf                                                                        
       Where CpgCia = :W_Cia                                                                        
         And CPGCMP = :W_Camp                                                                       
         And CpgCed = :WGldcte;                                                                     
       If W_ExisteR  = *Zeros;                                                                      
          PrEscribeXMes();                                                                          
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPr_RegistraPosteosAnterior...                                                                 
     P                 E                                                                            
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento que Graba Información en el Archivo Vtwlpgf         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrEscribeXMes...                                                                              
     P                 B                                                                            
     DPrEscribeXMes...                                                                              
     D                 Pi                                                                           
     Dw_AAAAMM         s              8a                                                            
     Dw_AuxYYMM        s              6a                                                            
     Ddsfecha          ds                                                                           
     Ddsyyyy                   1      4                                                             
     Ddsmm                     5      6                                                             
     Ddsdd                     7      8                                                             
     DdsfechaIso       ds                                                                           
     Ddsayyy                   1      4                                                             
     ddssep1                   5      5    Inz('-')                                                 
     Ddsmma                    6      7                                                             
     ddssep2                   8      8    Inz('-')                                                 
     Ddsdda                    9     10                                                             
     Dw_QSpgfds                            like(QSpgfds)                                            
     Dw_campana        s              4A                                                            
      /Free                                                                                         
       w_campana=W_Camp ;                                                                           
       Clear DsNewPlanGu;                                                                           
       Icedula = %trim(%editc(WGldcte:'P'));                                                        
       PgmNewPlanG(W_Cia:w_Zona:Icaso:Icedula:Ifecha:Orespuesta);                                   
       If Orespuesta = 'S';                                                                         
          w_campana = W_CampTem;                                                                    
       Endif;                                                                                       
       Clear w_count;                                                                               
       Exec SQl                                                                                     
       Select Count(0) Into :w_count                                                                
       From Vttspgf                                                                                 
       Where Spgcia = :W_Cia                                                                        
         And Spgcam = :W_Camp                                                                       
         And Spgced = :WGldcte ;                                                                    
       Clear w_AAAAMM;                                                                              
       Clear w_AuxYYMM;                                                                             
       Clear V_Valtra;                                                                              
       Clear w_AcuDtl;                                                                              
       Exec sql                                                                                     
       Declare csr4 cursor for                                                                      
        Select adrdpt, ADTNDÑ,                                                                      
               digits(adtrcc)||digits(adtryy)||digits(adtrmm)||digits(ADTRDD),                      
               ADTRAM                                                                               
        from pardtl                                                                                 
        Where ADCSTÑ = :WGldcte ;                                                                   
          Exec Sql                                                                                  
            Open csr4;                                                                              
          Exec Sql                                                                                  
            fetch csr4 Into :QAdrdpt,:qADTNDÑ,:w_AAAAMM,:QADTRAM;                                   
       w_AuxYYMM = w_AAAAMM;                                                                        
       Dow SqlCod = *Zeros;                                                                         
           Clear WADTNDÑ;                                                                           
           WADTNDÑ  = qADTNDÑ;                                                                      
           CAMBIARCq(WADTNDÑ);                                                                      
           Clear w_WADTNDÑ;                                                                         
           w_WADTNDÑ = WADTNDÑ;                                                                     
           Clear w_campAux;                                                                         
           If w_count = *Zeros;                                                                     
              w_campAux  = w_WADTNDÑ2;                                                              
           Else;                                                                                    
              w_campAux  = w_WADTNDÑ1;                                                              
           EndIf;                                                                                   
           IF w_campAux  = %Trim(w_campana);                                                        
                                                                                                    
              If w_AuxYYMM <> %subst(w_AAAAMM:1:6) and w_AcuDtl > *Zeros;                           
                 q_CPGNOM = 'Pago ya posteado';                                                     
                 Clear dsfechaIso;                                                                  
                 dsayyy = dsyyyy;                                                                   
                 dssep1 = '-';                                                                      
                 dsmma  = dsmm;                                                                     
                 dssep2 = '-';                                                                      
                 dsdda  = dsdd;                                                                     
                 w_QSpgfds = dsfechaIso;                                                            
                 Exec Sql                                                                           
                  Insert Into Votrea00/vtwCpgf Values(:W_Cia ,:WGldcte,                             
                  :W_Zona  ,                                                                        
                  :WGldsct,:W_Camp ,:w_AcuDtl,:w_AcuDtl, 0 ,                                        
                  User , :w_QSpgfds ,Current_time, :q_CPGNOM ,                                      
                   0       ,:w_AcuDtl , 0       , 0       , ' '     ,' '      );                    
                 Clear w_AcuDtl;                                                                    
                 Clear q_CPGNOM;                                                                    
              EndIf;                                                                                
              If QADTRAM < *Zeros;                                                                  
                 dsfecha =w_AAAAMM;                                                                 
                 w_AuxYYMM = w_AAAAMM;                                                              
                 QADTRAM = QADTRAM * -1;                                                            
                 w_AcuDtl  += QADTRAM;                                                              
              EndIf;                                                                                
           EndIf;                                                                                   
          Exec Sql                                                                                  
            fetch csr4 Into :QAdrdpt,:qADTNDÑ,:w_AAAAMM,:QADTRAM;                                   
       EndDO;                                                                                       
          Exec Sql                                                                                  
            Close csr4;                                                                             
       If w_AcuDtl > *Zeros;                                                                        
          Clear dsfechaIso;                                                                         
          dsayyy = dsyyyy;                                                                          
          dssep1 = '-';                                                                             
          dsmma  = dsmm;                                                                            
          dssep2 = '-';                                                                             
          dsdda  = dsdd;                                                                            
          w_QSpgfds = dsfechaIso;                                                                   
          q_CPGNOM = 'Pago ya posteado';                                                            
          Exec Sql                                                                                  
           Insert Into Votrea00/vtwCpgf Values(:W_Cia ,:WGldcte, :W_Zona  ,                         
           :WGldsct,:W_Camp ,:w_AcuDtl,:w_AcuDtl, 0 , User , :w_QSpgfds                             
           ,Current_time, :q_CPGNOM , 0 ,:w_AcuDtl , 0 , 0  , ' ' ,' ' );                           
          Clear w_AcuDtl;                                                                           
          Clear q_CPGNOM;                                                                           
       EndIf;                                                                                       
                                                                                                    
                                                                                                    
      /End-Free                                                                                     
     PPrEscribeXMes...                                                                              
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo Actualiza informe pago lideres                          //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrActualizaPagoLideres...                                                                     
     P                 B                                                                            
     DPrActualizaPagoLideres...                                                                     
     D                 Pi                                                                           
      /Free                                                                                         
       If Spgccr = 43;                                                                              
          QLpgVDE = Spgvds;                                                                         
        //QLpgVDE = Spgvps;                                                                         
          QLpgFde = Spgfbt;                                                                         
          Exec Sql                                                                                  
           Update votrea00/vtwCpgF set CPGVAE = :Spgvds                                             
           Where RRN(vtwCpgf) = (SELECT RRN(vtwCpgf)                                                
                            fROM Votrea00/vtwCpgf                                                   
                            Where CPGCIA = :W_Cia                                                   
                              And CPGCED = :WGldcte                                                 
                              And CPGZNA = :W_Zona                                                  
                              And CPGSEC = :WGldsct                                                 
                              Order By RRN(vtwCpgf) Desc                                            
                              Fetch First 1 Row Only) ;                                             
                                                                                                    
                      //                      ;                                                     
          Exec Sql                                                                                  
           Update Votrea00/VtwVrff Set VRFCAR =:Spgvds                                              
           Where RRN(VtwVrff) = (Select RRN(VtwVrff)                                                
                                 From Votrea00/VtwVrff                                              
                                 Where VRFCIA = :W_Cia                                              
                                   And VRFZNA = :W_Zona                                             
                                   And VRFCED = :WGldcte                                            
                                   And VRFIPC = ' '                                                 
                                   And VRFSEC = :WGldsct                                            
                                   Order By RRN(VtwVrff) Desc                                       
                                   Fetch First 1 Row Only) ;                                        
       Else;                                                                                        
          QLPGVDC = Spgvds;                                                                         
        //QLPGVDC = Spgvps;                                                                         
          QLpgFdc = Spgfbt;                                                                         
          Exec Sql                                                                                  
           Update votrea00/vtwCpgF set CPGVPC = :Spgvds                                             
           Where RRN(vtwCpgf) = (SELECT RRN(vtwCpgf)                                                
                            From Votrea00/vtwCpgf                                                   
                            Where CPGCIA = :W_Cia                                                   
                              And CPGCED = :WGldcte                                                 
                              And CPGZNA = :W_Zona                                                  
                              And CPGSEC = :WGldsct                                                 
                              Order By RRN(vtwCpgf) Desc                                            
                              Fetch First 1 Row Only) ;                                             
          Exec Sql                                                                                  
           Update Votrea00/VtwVrff Set VRFEFE =:Spgvds                                              
           Where RRN(VtwVrff) = (Select RRN(VtwVrff)                                                
                                 From Votrea00/VtwVrff                                              
                                 Where VRFCIA = :W_Cia                                              
                                   And VRFZNA = :W_Zona                                             
                                   And VRFCED = :WGldcte                                            
                                   And VRFSEC = :WGldsct                                            
                                   And VRFIPC = ' '                                                 
                                   Order By RRN(VtwVrff) Desc                                       
                                   Fetch First 1 Row Only) ;                                        
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrActualizaPagoLideres...                                                                     
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo Actualiza informe pago lideres                          //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrReteFuente...                                                                               
     P                 B                                                                            
     DPrReteFuente...                                                                               
     D                 Pi                                                                           
     DWNsuma           s             11  2 Inz(*Zeros)                                              
      /Free                                                                                         
       // Aplicar Descuento % Iva Retefuente Para Colombia                                          
       If W_Cia <= '002';                                                                           
          Clear W_sumVds;                                                                           
          Exec Sql                                                                                  
           Select IfNull(Sum(SPGVDS) , 0 ) Into :W_sumVds                                           
           From Votrea00/Vtmspgf                                                                    
           Where Spgcia = :W_Cia                                                                    
             And Spgzon = :W_Zona                                                                   
             And Spgcam = :W_Camp                                                                   
             And Spgced = :WGldcte;                                                                 
          If Not %Open(Vtwcphf);                                                                    
            Open Vtwcphf;                                                                           
          EndIf;                                                                                    
          Chain (W_Cia:WGldcte:W_Zona:W_Camp) Vtwcphf;                                              
          If Not %Found(Vtwcphf);                                                                   
             CPHCIA =W_Cia;                                                                         
             CPHCED =WGldcte;                                                                       
             CPHZNA =W_Zona;                                                                        
             CPHCMP =W_Camp;                                                                        
             CPHfec =%date();                                                                       
             CPHHor =%Time();                                                                       
             Write RegCph;                                                                          
             Clear wNsuma;                                                                          
             wNsuma = W_sumVds;                                                                     
             Pr_Cal_Retenciones(WNsuma);                                                            
             wdato = W_Cia+'-'+W_Camp+'-'+%trim(%editc(WGldcte:'P'))+'-'+                           
                     %trim(%editc(WNsuma:'P'))+'VTPSGBJR'+%char(%date());                           
             Exec Sql                                                                               
             Insert into gnpnalsof4/PRUEBALE Values(:wdato);                                        
             Chain (Cphcia:Cphced:Cphzna:Cphcmp) Vtwcphf;                                           
             If %Found(Vtwcphf);                                                                    
                Delete RegCph;                                                                      
             EndIf;                                                                                 
          ElseIf %Found(Vtwcphf) And (CPHfec < %date()                                              
             Or (CPHHor + %Minutes(1)) < %time());                                                  
             Delete RegCph;                                                                         
             CPHfec =%date();                                                                       
             CPHHor =%Time();                                                                       
             Write RegCph;                                                                          
             Clear wNsuma;                                                                          
             wNsuma = W_sumVds;                                                                     
             Pr_Cal_Retenciones(WNsuma);                                                            
             wdato = W_Cia+'-'+W_Camp+'-'+%trim(%editc(WGldcte:'P'))+'-'+                           
                     %trim(%editc(WNsuma:'P'))+'VTPSGBJR'+%char(%date());                           
             Exec Sql                                                                               
             Insert into gnpnalsof4/PRUEBALE Values(:wdato);                                        
             Chain (Cphcia:Cphced:Cphzna:Cphcmp) Vtwcphf;                                           
             If %Found(Vtwcphf);                                                                    
                Delete RegCph;                                                                      
             EndIf;                                                                                 
          EndIf;                                                                                    
          If %Open(Vtwcphf);                                                                        
            Close Vtwcphf;                                                                          
          EndIf;                                                                                    
       EndIf;                                                                                       
      /End-Free                                                                                     
     PPrReteFuente...                                                                               
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo que Valida el Pago                                      //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrValidarPago    B                                                                            
     DPrValidarPago    Pi              n                                                            
     DRetorno          s               n   Inz(*Off)                                                
     DNsumav           s             11  2 Inz(*Zeros)                                              
     Dw_Adtram         s             11  2 Inz(*Zeros)                                              
      /Free                                                                                         
       //Recupera Valor en cartera plan alas                                                        
       Clear Hvalor;                                                                                
       PrVlrPosteoCampaaPlanAlas(Hvalor);                                                           
                                                                                                    
       Trete = *Zeros;                                                                              
       Exec Sql                                                                                     
        Select IfNull(Sum(VRFVRR), 0)  into :Trete                                                  
        From Votrea00/Vtwvrff                                                                       
        Where Vrfcia =:W_Cia                                                                        
          And Vrfcam =:W_Camp                                                                       
          And Vrfced =:WGldcte                                                                      
          And Vrfipc = 'P';                                                                         
                                                                                                    
        Kvalor = *Zeros;                                                                            
        W_Valor1v = *Zeros;                                                                         
        W_Valor2v = *Zeros;                                                                         
        W_Valor3v = *Zeros;                                                                         
        PrInscReincorV();                                                                           
        PrPagosStencilV();                                                                          
        PrPagosBonosV();                                                                            
        Nsumav = *Zeros;                                                                            
        Nsumav = (W_Valor1V+W_Valor2V+W_Valor3V) - Trete;                                           
        If Nsumav <> Hvalor And Hvalor <> *Zeros;                                                   
         If Hvalor < Nsumav;                                                                        
            Clear W_sumVds;                                                                         
            w_ccr= 43;                                                                              
            If Spgccr = 43;                                                                         
               w_ccr = 42;                                                                          
            EndIf;                                                                                  
            Clear w_Adtram;                                                                         
            Exec Sql                                                                                
             Select IfNull(Sum(SPGVDS) , 0 )-IfNull( SUM((Adtram*(-1))) , 0 ) ,                     
                    IfNull( SUM((Adtram*(-1))) , 0 )                                                
             Into :W_sumVds, :w_Adtram                                                              
             From Vtmspgf                                                                           
             Left join Pardtl On AdcstÑ  = Spgced                                                   
             And ADRDPT = Spgccr                                                                    
             And Substr(ADTNDÑ, (Length(Trim(ADTNDÑ))-3), 4)                                        
                                 = Trim(Spgcam) And Adtram < 0                                      
             Where Spgcia = :Spgcia And                                                             
                   Spgzon = :Spgzon And                                                             
                   Spgcam = :Spgcam And                                                             
                   Spgsec = :Spgsec And                                                             
                   Spgtrr = :Spgtrr And                                                             
                   Spgced = :Spgced And                                                             
                   Spgccr = :w_ccr                                                                  
                   And AdtndÑ Not Like '%Dt Cd Líder Ef%'                                           
                   And AdtndÑ Not Like '%Dcto Cial Cond C%';                                        
          Kvalor = (Nsumav - (Hvalor-w_Adtram) - W_sumVds - w_Adtram );                             
      *   Kvalor = (Nsumav - Hvalor);                                                               
          If Kvalor > *Zeros;                                                                       
           Retorno = *On;                                                                           
          Endif;                                                                                    
         Endif;                                                                                     
        Endif;                                                                                      
                                                                                                    
       Return Retorno;                                                                              
      /End-Free                                                                                     
     PPrValidarPago    E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo Pr_ValidaRangoRetener:                                  //                        
      //          Valida si el valor a pagar se encuentra en el rago      //                        
      //          parametrico en el archico VtmVrgf  para la compañia//                             
      //////////////////////////////////////////////////////////////////////                        
     PPr_ValidaRangoRetener...                                                                      
     P                 B                                                                            
     DPr_ValidaRangoRetener...                                                                      
     D                 Pi                                                                           
     Dw_VlrPagoVerifi                11  2                                                          
     Dw_PorcentageRet                 6  3                                                          
      /Free                                                                                         
       //Verifica si Pago se ecuentra dentro del parametrizado a Retener                            
       Clear Wsql;                                                                                  
       Clear w_PorcentageRet;                                                                       
       Wsql= 'Select VrgPor, Vrgest ' +                                                             
             ' From Votrea'+ W_Cia +'/VtmVrgf '+                                                    
             ' Where ' + %trim(%editc(w_VlrPagoVerifi:'P'))+                                        
             ' BetWeen VRGVRI and VRGVRF';                                                          
       Exec Sql Prepare Csr1 From :Wsql;                                                            
       Exec Sql Declare Csr1 Cursor For Csr1;                                                       
       Exec Sql Open Csr1;                                                                          
       Exec Sql Fetch Csr1 Into :w_PorcentageRet, :Vrgest;                                          
       Exec Sql Close Csr1;                                                                         
                                                                                                    
       If Vrgest = 'D';                                                                             
        Clear w_PorcentageRet;                                                                      
       Endif;                                                                                       
      /End-Free                                                                                     
     PPr_ValidaRangoRetener...                                                                      
     P                 E                                                                            
                                                                                                    
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Prototipo Pr_InsertaRetencionVtwvrff:                             //                        
      //          Inserta valores a retener para luego en el posteo       //                        
      //          realizar descuento                                      //                        
      //////////////////////////////////////////////////////////////////////                        
     PPr_InsertaRetencionVtwvrff...                                                                 
     P                 B                                                                            
     DPr_InsertaRetencionVtwvrff...                                                                 
     D                 Pi                                                                           
      /Free                                                                                         
           W_Vrffec = %Timestamp();                                                                 
           Chain (W_Cia:W_Camp:W_Zona:WGldcte:WGldsct:' ':w_Vrffec) VtwVrff;                        
              If Not %Found(VtwVrff);                                                               
                 Vrfcia = W_Cia  ;                                                                  
                 Vrfcam = W_Camp ;                                                                  
                 Vrfzna = W_Zona ;                                                                  
                 Vrfced = WGldcte;                                                                  
                 Vrfsec = WGldsct;                                                                  
                 If w_SaldoPen > *Zeros;                                                            
                    Vrfvrr = w_SumVlrLq;                                                            
                 Else;                                                                              
                    Vrfvrr = w_SumVlrRet;                                                           
                 EndIf;                                                                             
                 Vrfprr = W_PorcentaRetencion * 100;                                                
                 Vrfvli = w_VlrALiquidado ;                                                         
                 If Spgccr = 42;                                                                    
                    Vrfcar = w_VlrALiquidado ;                                                      
                 ELse;                                                                              
                    Vrfefe = w_VlrALiquidado ;                                                      
                 EndIf;                                                                             
                 vrfccr= Spgccr;                                                                    
                 Vrfipc = wx_Vrfipc;                                                                
                 w_Vrffec = %Timestamp();                                                           
                 Vrffec = w_Vrffec;                                                                 
                 If Vrfvrr > *Zeros;                                                                
                    Write RegVrf;                                                                   
                 EndIf;                                                                             
                                                                                                    
              EndIf;                                                                                
      /End-Free                                                                                     
     PPr_InsertaRetencionVtwvrff...                                                                 
     P                 E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////////                    
      //Protrotipo PrVlrPosteoCampaaPlanAlas   : Recupara valor pago en       //                    
      // cartera                                                              //                    
      //////////////////////////////////////////////////////////////////////////                    
       Dcl-proc PrVlrPosteoCampaaPlanAlas;                                                          
       Dcl-Pi *n;                                                                                   
        w_ttlPogoCart Packed(11:2);                                                                 
       End-Pi;                                                                                      
       Dcl-s w_CampNewpl Char(4);                                                                   
       Dcl-s w_VlrAcuDtlTradi Packed(11:2);                                                         
       Clear w_ttlPogoCart;                                                                         
       //Verifica si Zona es parte de nuevo plan de Guías                                           
       w_CampNewpl=W_Camp;                                                                          
       Clear DsNewPlanGu;                                                                           
       Icedula = %trim(%editc(WGldcte:'P'));                                                        
       PgmNewPlanG(W_Cia:W_Zona:Icaso:Icedula:Ifecha:Orespuesta);                                   
       If Orespuesta = 'S';                                                                         
        //w_CampNewpl = W_CampTem;                                                                  
        w_CampNewpl=W_CampAnt;                                                                      
       Endif;                                                                                       
       Clear V_Valtra;                                                                              
       Clear w_VlrAcuDtlTradi;                                                                      
       Exec sql                                                                                     
       Declare csr5 cursor for                                                                      
       Select *                                                                                     
       from pardtl                                                                                  
       Where AdcstÑ = :WGldcte                                                                      
         And (ADRDPT = 042 Or ADRDPT = 043)                                                         
         And Substr(ADTNDÑ, (Length(Trim(ADTNDÑ))-3), 4) = Trim(:W_Camp)                            
         And Adtram < 0;                                                                            
       Exec Sql Open csr5;                                                                          
       Exec Sql fetch csr5 Into :Fields3;                                                           
       Dow SqlCod = *Zeros;                                                                         
         Clear WADTNDÑ;                                                                             
         WADTNDÑ  = qADTNDÑ;                                                                        
         CAMBIARCq(WADTNDÑ);                                                                        
         Clear w_WADTNDÑ;                                                                           
         w_WADTNDÑ = %Trim(WADTNDÑ);                                                                
         IF w_WADTNDÑ2 = %Trim(w_CampNewpl) And QADTRAM < *Zeros;                                   
            w_VlrAcuDtlTradi  += (QADTRAM * -1);                                                    
         EndIf;                                                                                     
         Exec Sql fetch csr5 Into :Fields3;                                                         
       EndDO;                                                                                       
       Exec Sql Close csr5;                                                                         
       w_ttlPogoCart = w_VlrAcuDtlTradi;                                                            
       End-Proc PrVlrPosteoCampaaPlanAlas;                                                          
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta por Inscripciones o Reincorporaciones      //                        
      //Estrela Efectivas                                                 //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrInscReincorT   B                                                                            
     DPrInscReincorT   Pi                                                                           
      /Free                                                                                         
       Clear WValor1T;                                                                              
       Clear W_Valor1T;                                                                             
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(Sum(Iegvpg), 0) ' +                                                    
              'From VotreA00/Vtwiegf ' +                                                            
              'Where Iegcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'IegZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Iegscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                            
              'Iegcdg = ' + %trim(%char(WGldcte))            + ' And ' +                            
              'Iegcmp = ' + $Coma + %trim(PCamR)     + $Coma + ' And ' +                            
              'IegItp = ''E''';                                                                     
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor4v1 From :Wsql;                                                              
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor4v1 Scroll Cursor For Cursor4v1;                                             
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor4v1;                                                                            
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor4v1 Into :WValor1T;                                                            
                                                                                                    
         W_Valor1T = *Zeros;                                                                        
        If SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         W_Valor1T = WValor1T;                                                                      
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor4v1;                                                                           
                                                                                                    
      /End-Free                                                                                     
     PPrInscReincorT   E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta Pagos Stencil Por Cumplimiento de Pedidos  //                        
      //Facturads Segun  su Clasificacion de Alas                         //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrPagosStencilT  B                                                                            
     DPrPagosStencilT  Pi                                                                           
     DWcodAla          s              4  0  Inz(*Zeros)                                             
      /Free                                                                                         
       WcodAla = *Zeros;                                                                            
       Clear WValor2T;                                                                              
       Clear W_Valor2T;                                                                             
       Exec Sql                                                                                     
        Select Cnacod Into :WcodAla                                                                 
        From Votrea00/Vtmcnaf                                                                       
        Where Cnacia = :Icia And                                                                    
              Cnazna = :Izona And                                                                   
              Cnacmp = :PCamR And                                                                   
              Cnasec = :WGldsct                                                                     
        Order By Rrn(Vtmcnaf) Desc                                                                  
        Fetch First 1 Row Only;                                                                     
                                                                                                    
       Wsql = *Blanks;                                                                              
       If WcodAla = *Zeros;                                                                         
        Wsql = 'Select IfNull(Sum(Dggvlr), 0) ' +                                                   
               'From VotreA00/Vttdggf ' +                                                           
               'Where Dggcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
               'DggZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                           
               'Dggscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                           
               'Dggcdg = ' + %trim(%char(WGldcte))            + ' And ' +                           
               'Dggcmp = ' + $Coma + %trim(PCamR)     + $Coma;                                      
       Else;                                                                                        
        Wsql = 'Select IfNull(Sum(Dggvlr), 0) ' +                                                   
               'From VotreA00/Vttdggf ' +                                                           
               'Where Dggcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                           
               'DggZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                           
               'Dggscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                           
               'Dggcdg = ' + %trim(%char(WGldcte))            + ' And ' +                           
               'Dggcmp = ' + $Coma + %trim(PCamR)     + $Coma;                                      
         //    'Dggcga = ' + %trim(%char(WcodAla));                                                 
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor2v1 From :Wsql;                                                              
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor2v1 Scroll Cursor For Cursor2v1;                                             
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor2v1;                                                                            
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor2v1 Into :WValor2T;                                                            
                                                                                                    
        W_Valor2T = *Zeros;                                                                         
        If SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         W_Valor2T = WValor2T;                                                                      
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor2v1;                                                                           
                                                                                                    
      /End-Free                                                                                     
     PPrPagosStencilT  E                                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //Procedimiento Consulta Pagos Bonos (Conseguir o Conservar )       //                        
      //////////////////////////////////////////////////////////////////////                        
     PPrPagosBonosT    B                                                                            
     DPrPagosBonosT    Pi                                                                           
      /Free                                                                                         
       Clear WValor3T;                                                                              
       Clear W_Valor3T;                                                                             
       Wsql = *Blanks;                                                                              
       Wsql = 'Select IfNull(Sum(Pgbvlr), 0) ' +                                                    
              'From VotreA00/VttPgbf ' +                                                            
              'Where Pgbcia= ' + $Coma + %trim(Icia) + $Coma + ' And ' +                            
              'PgbZng = ' + $Coma + %trim(IZona)     + $Coma + ' And ' +                            
              'Pgbscg = ' + $Coma + %trim(WGldsct)   + $Coma + ' And ' +                            
              'Pgbcdg = ' + %trim(%char(WGldcte))            + ' And ' +                            
              'Pgbcmp = ' + $Coma + %trim(PCamR) + $Coma;                                           
                                                                                                    
       Exec Sql                                                                                     
         Prepare Cursor3v1 From :Wsql;                                                              
                                                                                                    
       Exec Sql                                                                                     
         Declare Cursor3v1 Scroll Cursor For Cursor3v1;                                             
                                                                                                    
       Exec Sql                                                                                     
         Open Cursor3v1;                                                                            
                                                                                                    
       Exec Sql                                                                                     
         fetch Cursor3v1 Into :WValor3T;                                                            
                                                                                                    
        W_Valor3T = *Zeros;                                                                         
        If SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         W_Valor3T = WValor3T;                                                                      
        EndIf;                                                                                      
                                                                                                    
       Exec Sql                                                                                     
         Close cursor3v1;                                                                           
                                                                                                    
      /End-Free                                                                                     
     PPrPagosBonosT    E                                                                            
