      ***********************************************************************                       
      ** Programa      : VTPSGC3R                                          **                       
      ** Descripciòn   : PGM Consulta Control Retefuente Guías             **                       
      ** Autor         : Leonel Mauricio Parra Suárez-PersonalSoft S.A.    **                       
      ** Responsable   : Julian Ortega                                     **                       
      ** Fecha Creación: 03 de Abril de 2017                               **                       
      ***********************************************************************                       
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
     Hdatfmt(*Iso)                                                                                  
                                                                                                    
     **Definicion de informes                                                                       
     FVTPSGC1I  o    e             Printer Oflind(*In99) Usropn                                     
     FVTPSGC1P  Cf   e             Workstn                                                          
     F                                     InfDs( infDsp )                                          
      **Definición de Variables de Trabajo                                                          
     DUser             s             10a   Inz(*User)                                               
     D$C               c                   ''''                                                     
     DXInd             s              1A                                                            
     DSqlStr           s           5000a   Inz(*Blanks)                                             
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
     Dw_FechaI         s               D                                                            
     Dw_FechaF         s               D                                                            
                                                                                                    
     D                sDs                                                                           
     Dnomp               *Proc                                                                      
                                                                                                    
     D InfDsp          DS                                                                           
     D                                     Qualified                                                
     D   key                          1A   Overlay( InfDsp: 369 )                                   
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
                                                                                                    
     DFields4        e Ds                  Extname(VtwLpgf:RegLpg)                                  
     D                                     Prefix(Q)                                                
     DResulSetZon      Ds                  Occurs(999) Qualified                                    
     DSelec                           1a                                                            
     DW_ZONA                          3a                                                            
     ** Definiciòn de Estructuras de Trabajo                                                        
     DIndic            Ds                  Based(Ptr)                                               
     DDAyuda                           N   Overlay(Indic:1)                                         
     DSalir                            N   Overlay(Indic:3)                                         
     DGenerar                          N   Overlay(Indic:5)                                         
     DCancelar                         N   Overlay(Indic:12)                                        
                                                                                                    
      **Definición de Procedimientos                                                                
     D InformeR        Pr                                                                           
     DPrSpool          pr                                                                           
     DPrFileNew        pr                                                                           
     Dcomando          pr                  extpgm('QCMDEXC')                                        
     D                               20    options(*varsize)                                        
     D                                     const                                                    
     Dl                              15p 5                                                          
     DCompaÑia         Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10                                                             
     DPres                            2                                                             
     DKciaR                          10                                                             
     DPgmPais1         Pr                  Extpgm('VOTREP00/VTMGOI8R')                              
     DXcia                            3                                                             
     Dwpais                          30                                                             
                                                                                                    
     DPgmCampaÑa       Pr                  Extpgm('VOTREP00/VTMGOIFR')                              
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
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /free                                                                                         
                                                                                                    
       Dow *In03 = *off;                                                                            
           Clear Blanco;                                                                            
           Write Blanco;                                                                            
           Exfmt principal;                                                                         
           Clear Perror;                                                                            
           If Popppais=1;                                                                           
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
              Iter;                                                                                 
           EndIf;                                                                                   
           If *In03=*on;                                                                            
              Leave;                                                                                
           EndIf;                                                                                   
           If *In12  =*on;                                                                          
              Clear principal;                                                                      
              *In12 =*off;                                                                          
              iter;                                                                                 
           EndIf;                                                                                   
           If *in05 =*on;                                                                           
              If ValidaCamps = *Off;                                                                
                    InformeR();                                                                     
                    PMSJ1='    La Consulta se ha generado Exitosamente]]';                          
                    Pfile = 'VTWIRTEF y VTWICPGF';                                                  
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
        ElseIf PFECINI = *Zeros;                                                                    
           *in31=*On;                                                                               
           Perror = 'Ingrese Fecha Inicial';                                                        
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
        ElseIf PFECINI > *Zeros;                                                                    
           Monitor;                                                                                 
           Clear w_FechaI;                                                                          
           w_FechaI = %date(PFECini);                                                               
           on-Error;                                                                                
           *in31=*On;                                                                               
           Perror = 'Fecha Inicial no valida';                                                      
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
           EndMon;                                                                                  
        EndIf;                                                                                      
        If PFECfin = *Zeros;                                                                        
           *in32=*On;                                                                               
           Perror = 'Ingrese Fecha Final';                                                          
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
        ElseIf PFECfin > *Zeros;                                                                    
           Monitor;                                                                                 
           Clear w_Fechaf;                                                                          
           w_FechaF = %date(PFECfin);                                                               
           on-Error;                                                                                
           *in32=*On;                                                                               
           Perror = 'Fecha Final no valida';                                                        
           IndCam = *On;                                                                            
           Return IndCam;                                                                           
           EndMon;                                                                                  
        EndIf;                                                                                      
        If PFECfin < PFECini;                                                                       
           *in32=*On;                                                                               
           Perror = 'Fecha inicial mayor a fecha final';                                            
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
         open Vtpsgc1I;                                                                             
         PFECHA=%char(%date());                                                                     
         PPGM=nomp;                                                                                 
         IFECHAINI = w_FechaI;                                                                      
         IFECHAFIN = w_FechaF;                                                                      
         Write Titulos;                                                                             
                                                                                                    
         SqlStr = *Blanks;                                                                          
         SqlStr = 'Select distinct cedula, nombre, '+                                               
                  'ifnull(salpena, 0.00) + ifnull(salpenxr, 0.00) as salpena, '+                    
                  'vlrliquido , retencion, vlrliquido-retencion as vlrcsigcar '+                    
                  'From(select mlcstÑ as cedula, '+                                                 
                  'Mlname as  nombre, (select ifnull(vrtvlr, 0.00) '+                               
                  'from vtwvrtf where vrtced=cpgced )as salpena, '+                                 
                  '(select ifnull(sum(cpgvll) , 0.00) from votrea00.vtwcpgf '+                      
                  'where cpgced=mlcstÑ and int(cpgfec) between '+%char(PFECIni)+                    
                  ' and '+ %char(PFECfin)+' ) as vlrliquido     , '+                                
                  '(select ifnull(sum(vrfvrr), 0.00 ) from votrea00.vtwvrff '+                      
                  'where vrfced=mlcstÑ '+                                                           
                  '  and vrffec >= '+$c+%char(w_FechaI)+' 01:01:01.000000'+$c+                      
                  '  and vrffec <= '+$c +%char(w_FechaF)+' 23:59:59.999999'+$c+                     
                  ') as retencion,'+                                                                
                  '(select ifnull(sum(cpgspr), 0.00) from votrea00.vtwcpgf '+                       
                  'where cpgced=mlcstÑ )as salpenxr, '+                                             
                  '(select ifnull(sum(cpgvds) , 0.00) from votrea00.vtwcpgf '+                      
                  ' where cpgced=mlcstÑ and int(cpgfec) between '+                                  
                   %char(PFECIni)+' and '+%char(PFECfin)+' ) as vlrcsigcar '+                       
                  'From votrea00.vtwcpgf  ' +                                                       
                  'Inner join opf'+compania+'.lmllst01 on mlcstÑ=cpgced '+                          
                  'Where int(cpgfec) between '+%char(PFECIni)+' and ' +                             
                  %char(PFECfin)+ ' Order by mlcstÑ asc) as tbl_resumen '+                          
                  ' where (salpena > 0.0 Or retencion > 0.0 )'+                                     
                  'Order by cedula asc ';                                                           
                                                                                                    
                                                                                                    
         Exec Sql                                                                                   
          Prepare Cur3 From :SqlStr;                                                                
                                                                                                    
         Exec Sql                                                                                   
          Declare Cursor3 Cursor For Cur3;                                                          
                                                                                                    
        Exec SQL                                                                                    
          open cursor3;                                                                             
                                                                                                    
        Exec SQL                                                                                    
          fetch cursor3 Into :QLpgcdg,:QLpgnom,:QLpgtds,:QLpgvde,:QLpgvdc,                          
                             :QLpgvrc;                                                              
        Dow sqlcod=*zeros;                                                                          
            PCODIIN   =QLpgcdg;                                                                     
            INOMBRE   =QLpgnom;                                                                     
            ISALPENA  =QLpgtds;                                                                     
            IVALLIQ   =QLpgvde;                                                                     
            IVLRRET   =QLpgvdc;                                                                     
            IVALCONCAR=QLpgvrc;                                                                     
            If *In99=*on;                                                                           
               PPGM=nomp;                                                                           
               IFECHAINI = w_FechaI;                                                                
               IFECHAFIN = w_FechaF;                                                                
               Write Titulos;                                                                       
               *In99=*off;                                                                          
            EndIf;                                                                                  
            Write Detalle;                                                                          
            Exec SQL                                                                                
            fetch cursor3 Into :qLpgcdg,:QLpgnom,:QLpgtds,:QLpgvde,:QLpgvdc,                        
                             :QLpgvrc;                                                              
        EndDo;                                                                                      
        Write PPIEPAG;                                                                              
        PrFileNew();                                                                                
                                                                                                    
        Exec SQL                                                                                    
          Close cursor3;                                                                            
                                                                                                    
       close Vtpsgc1I;                                                                              
      /END-FREE                                                                                     
     P InformeR        E                                                                            
                                                                                                    
     P*--------------------------------------------------                                           
     P* Procedure Name: PrSpool                                                                     
     P* Purpose       : Ir al Spool                                                                 
     P*--------------------------------------------------                                           
     PPrSpool          B                                                                            
     DPrSpool          PI                                                                           
      ** Definición de Variables de Trabajo                                                         
     DCmd              s             20                                                             
     DLong             s             15p 5                                                          
      /Free                                                                                         
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
                                                                                                    
       SqlStr = 'Drop Table ' + %Trim(User) + '/VTWICPGF';                                          
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Create Table ' + %Trim(User) + '/VTWICPGF As (' +                                  
                'select * from votrea00.vtwcpgf    ' +                                              
                'where  cpgfec >= '+$c+%char(w_FechaI)+$c+                                          
                ' and cpgfec <= '+$c +%char(w_FechaF)+$c;                                           
       SqlStr = %trim(SqlStr) + ' ) With Data';                                                     
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Drop Table ' + %Trim(User) + '/VTWIRTEF';                                          
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
                                                                                                    
       SqlStr = *Blanks;                                                                            
       SqlStr = 'Create Table ' + %Trim(User) + '/VTWIRTEF As (' +                                  
                  'Select distinct cedula, nombre, '+                                               
                  'ifnull(salpena, 0.00) + ifnull(salpenxr, 0.00) as salpena, '+                    
                  'vlrliquido, retencion, vlrliquido-retencion as vlrcsigcar '+                     
                  'From(select mlcstÑ as cedula, '+                                                 
                  'Mlname as  nombre, (select ifnull(vrtvlr, 0.00) '+                               
                  'from vtwvrtf where vrtced=cpgced )as salpena, '+                                 
                  '(select ifnull(sum(cpgvll) , 0.00) from votrea00.vtwcpgf '+                      
                  'where cpgced=mlcstÑ and int(cpgfec) between '+%char(PFECIni)+                    
                  ' and '+ %char(PFECfin)+' ) as vlrliquido     , '+                                
                  '(select ifnull(sum(vrfvrr), 0.00 ) from votrea00.vtwvrff '+                      
                  'where vrfced=mlcstÑ '+                                                           
                  '  and vrffec >= '+$c+%char(w_FechaI)+' 01:01:01.000000'+$c+                      
                  '  and vrffec <= '+$c +%char(w_FechaF)+' 23:59:59.999999'+$c+                     
                  ') as retencion,'+                                                                
                  '(select ifnull(sum(cpgspr), 0.00) from votrea00.vtwcpgf '+                       
                  'where cpgced=mlcstÑ )as salpenxr, '+                                             
                  '(select ifnull(sum(cpgvds) , 0.00) from votrea00.vtwcpgf '+                      
                  ' where cpgced=mlcstÑ and int(cpgfec) between '+                                  
                   %char(PFECIni)+' and '+%char(PFECfin)+' ) as vlrcsigcar '+                       
                  'From votrea00.vtwcpgf  ' +                                                       
                  'Inner join opf'+compania+'.lmllst01 on mlcstÑ=cpgced '+                          
                  'Where int(cpgfec) between '+%char(PFECIni)+' and ' +                             
                  %char(PFECfin)+ ' Order by mlcstÑ asc) as tbl_resumen '+                          
                  ' where (salpena > 0.0 Or retencion > 0.0 )'+                                     
                  'Order by cedula asc ';                                                           
                                                                                                    
       SqlStr = %trim(SqlStr) + ' ) With Data';                                                     
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :SqlStr;                                                                 
       Clear  SqlStr;                                                                               
       SqlStr = ' ALTER TABLE ' + %Trim(User) + '/VTWIRTEF'+                                        
                ' ADD COLUMN cedula     NUMERIC(9, 0) NOT HIDDEN'+                                  
                ' ADD COLUMN nombre     CHARACTER(30)  NOT HIDDEN'+                                 
                ' ADD COLUMN salpena    NUMERIC(11, 2) NOT HIDDEN'+                                 
                ' ADD COLUMN vlrliquido NUMERIC(11, 2) NOT HIDDEN'+                                 
                ' ADD COLUMN retencion  NUMERIC(11, 2) NOT HIDDEN'+                                 
                ' ADD COLUMN vlrcsigcar NUMERIC(11, 2) NOT HIDDEN';                                 
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
       Clear  SqlStr;                                                                               
                //* Estableciendo las etiquetas de columna para USER.VTWIRTEF /*                    
       SqlStr = 'LABEL ON COLUMN '+%Trim(User)+'/VTWIRTEF ( '+                                      
                ' CEDULA     TEXT IS '+ $C +'Cédula'+$C+' ,'+                                       
                ' NOMBRE     TEXT IS '+ $C +'Nombre'+$C+' ,'+                                       
                ' SALPENA    TEXT IS '+ $C +'Saldo Pendiente Anterior'+$C +' ,'+                    
                ' VLRLIQUIDO TEXT IS '+ $C +'Valor Liquidado'+$C+' ,'+                              
                ' RETENCION  TEXT IS '+ $C +'Retención'+ $C +' ,'+                                  
                ' VLRCSIGCAR TEXT IS '+ $C +'Valor Consignar Cartera'+$C+')';                       
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
       Clear  SqlStr;                                                                               
               //* Estableciendo las cabeceras de columna para USER.VTWIRTEF */                     
       SqlStr ='LABEL ON COLUMN '+ %Trim(User)+'/VTWIRTEF ('+                                       
               ' CEDULA     IS '+ $C +'Cédula'+ $C +' ,'+                                           
               ' NOMBRE     IS '+ $C +'Nombre'+ $C +' ,'+                                           
               ' SALPENA    IS '+ $C +'Saldo Pendiente Anterior'+$C +' ,'+                          
               ' VLRLIQUIDO IS '+ $C +'Valor Liquidado'+$C+' ,'+                                    
               ' RETENCION  IS '+ $C +'Retención'+ $C +' ,'+                                        
               ' VLRCSIGCAR IS '+ $C +'Valor Consignar Cartera'+$C+')';                             
       Exec Sql                                                                                     
        Execute Immediate :SqlStr;                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrFileNew        E                                                                            
