      **********************************************************************                        
      ** Programa      : VTPSGC5R                                         **                        
      ** Descripción   : Generacion de Archivo de Control de Pagos Guias  **                        
      **                 Tradicional                                      **                        
      ** Proyecto      : Plan Lideres Tradicional                         **                        
      ** Autor         : (AparraQ)                                        **                        
      ** Fecha Creación: Enero de 2018                                    **                        
      **********************************************************************                        
      ** Modulo: Compilar como Módulo con opción 15                       **                        
      ** CRTPGM PGM(VOTREP00/VTPSGC5R)                                    **                        
      ** MODULE(VOTREP00/VTPSGC5R VOTREP00/VTSPGS7R)                      **                        
      **********************************************************************                        
                                                                                                    
      *Directivas de Compilación                                                                    
     HOption(*NoDebugIO)                                                                            
     HDatfmt(*DMY)                                                                                  
                                                                                                    
      *Definición de Arcivos                                                                        
     FVTHCGTF   O  A E           K Disk                                                             
     FVTMPARF   IF   E           K Disk                                                             
     FLSLMAN01  IF   E           K Disk                                                             
     FLMLLST01  IF   E           K Disk                                                             
     FVTMSMPF   IF   E           K Disk                                                             
     FVTMRTGF   IF   E           K Disk                                                             
                                                                                                    
      *Parametro de Entrada al Programa (Entry)                                                     
     DMain             Pr                  Extpgm('VTPSGC5R')                                       
     DpCia                            3A                                                            
     DpZona                           3A                                                            
     DpCampa                          4A                                                            
                                                                                                    
     DMain             Pi                                                                           
     DpCia                            3A                                                            
     DpZona                           3A                                                            
     DpCampa                          4A                                                            
                                                                                                    
      *Definición estructura del sistema                                                            
     D Estado_Pgm     SDs                                                                           
     D   Nom_Pgm         *PROC                                                                      
     D   Msg_ErrID            40     46                                                             
     D   Msg_Err              91    169                                                             
     D   Nom_Job             244    253                                                             
     D   Usuario             254    263                                                             
     D   Nro_Job             264    269                                                             
     DSig_sys                195    196  0                                                          
     DAÑo_sys                280    281  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPrInicial        Pr                                                                           
     DPrProceso        Pr                                                                           
     DPrAsignaVlrs     Pr                                                                           
     DPrLimpiarVbles   Pr                                                                           
                                                                                                    
     DGuiasLiqCampa    Pr                  Extproc('VTSPGS7R')                                      
     DPciak                          10                                                             
     DPidl                           30                                                             
     DPidG                           15  0                                                          
     DPCamp                           4                                                             
     DPSec                            1                                                             
     DPTer                            1                                                             
     DPtitcamp                       60                                                             
     DPtitnamel                      60                                                             
     DPtitdsopr                      60                                                             
     DPtitzona1                      60                                                             
     DPtitfulac                      60                                                             
     DPtitmlpln                      60                                                             
     DPtitpanta                     200                                                             
     DPtitsecto                      60                                                             
     DPtitcedgi                      60                                                             
     DPtitnomgi                      60                                                             
     DPtotales                       60                                                             
     DPtotprest                      60                                                             
     DPtotdespr                      60                                                             
     DPtitbotn1                      60                                                             
     DPtitbotn2                      60                                                             
     DPtitbotn3                      60                                                             
     DPtitbotn4                      60                                                             
     DPnamelider                     30                                                             
     DPnameGuia                      30                                                             
     DPdiasoper                      15                                                             
     DPzonaL                          3                                                             
     DPfechaact                      30                                                             
     DPmailplan                       1                                                             
     DPtotalSum                      15                                                             
     DPtitpanta1                     60                                                             
     DPtitpanta2                     60                                                             
     DPstatus                         1                                                             
     DPdestatus                     120                                                             
     DPtitbotn5                      60                                                             
     DPtitCmpPto                     60                                                             
     DPtitActPto                     60                                                             
     DPCamPto                         4                                                             
     DPActPto                        10                                                             
     DPtitCmpPto1                    60                                                             
     DPtitActPto1                    60                                                             
     DPCamPtoA                        4                                                             
     DPActPtoA                       10                                                             
     DPtitTerri                      60                                                             
     DPtitActPtoZR                   60                                                             
     DPActPtoZR                      10                                                             
     DPtitbotn6                      60                                                             
     DPtitConta                      60                                                             
     DPNumeroCondi                    3  0                                                          
     DPtitPagEfec                    60                                                             
     DPValPagEfec                    14                                                             
     DPtitDesActC                    60                                                             
     DPTotalActCre                   14                                                             
     DPTituFecPago                   60                                                             
     DPFechaPago                     10                                                             
     DPTitPagoDesp                  100                                                             
     DPtitbotn7                      60                                                             
     DPtitRtf1                      100                                                             
     DPtitRtf2                      100                                                             
     DPtitRtf3                      100                                                             
     DPtitRtf4                      100                                                             
     DPtitRtf5                      100                                                             
     DPtitRtf6                      100                                                             
     DPVlrRtf1                       60                                                             
     DPVlrRtf2                       60                                                             
     DPVlrRtf3                       60                                                             
     DPVlrRtf4                       60                                                             
     DPVlrRtf5                       60                                                             
     DPVlrRtf6                       60                                                             
                                                                                                    
     DConversionID     Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DTcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
      *Definición de Variables de Trabajo                                                           
     DResulSetDesc     Ds                  Qualified                                                
     DPdescrip                      100                                                             
     DPnumeroL                        5  0                                                          
     DPvalorL                        20  0                                                          
     DPCodLiq                         3  0                                                          
     DPNumCon                         3  0                                                          
     DPTitPagC                        1                                                             
     DPRrn                            5  0                                                          
                                                                                                    
     DPciak            S             10                                                             
     DPidl             S             30                                                             
     DPidG             S             15  0                                                          
     DPSec             S              1                                                             
     DPTer             S              1                                                             
     DPtitcamp         S             60                                                             
     DPtitnamel        S             60                                                             
     DPtitdsopr        S             60                                                             
     DPtitzona1        S             60                                                             
     DPtitfulac        S             60                                                             
     DPtitmlpln        S             60                                                             
     DPtitpanta        S            200                                                             
     DPtitsecto        S             60                                                             
     DPtitcedgi        S             60                                                             
     DPtitnomgi        S             60                                                             
     DPtotales         S             60                                                             
     DPtotprest        S             60                                                             
     DPtotdespr        S             60                                                             
     DPtitbotn1        S             60                                                             
     DPtitbotn2        S             60                                                             
     DPtitbotn3        S             60                                                             
     DPtitbotn4        S             60                                                             
     DPnamelider       S             30                                                             
     DPnameGuia        S             30                                                             
     DPdiasoper        S             15                                                             
     DPzonaL           S              3                                                             
     DPfechaact        S             30                                                             
     DPmailplan        S              1                                                             
     DPtotalSum        S             15                                                             
     DPtitpanta1       S             60                                                             
     DPtitpanta2       S             60                                                             
     DPstatus          S              1                                                             
     DPdestatus        S            120                                                             
     DPtitbotn5        S             60                                                             
     DPtitCmpPto       S             60                                                             
     DPtitActPto       S             60                                                             
     DPCamPto          S              4                                                             
     DPActPto          S             10                                                             
     DPtitCmpPto1      S             60                                                             
     DPtitActPto1      S             60                                                             
     DPCamPtoA         S              4                                                             
     DPActPtoA         S             10                                                             
     DPtitTerri        S             60                                                             
     DPtitActPtoZR     S             60                                                             
     DPActPtoZR        S             10                                                             
     DPtitbotn6        S             60                                                             
     DPtitConta        S             60                                                             
     DPNumeroCondi     S              3  0                                                          
     DPtitPagEfec      S             60                                                             
     DPValPagEfec      S             14                                                             
     DPtitDesActC      S             60                                                             
     DPTotalActCre     S             14                                                             
     DPTituFecPago     S             60                                                             
     DPFechaPago       S             10                                                             
     DPTitPagoDesp     S            100                                                             
     DPtitbotn7        S             60                                                             
     DPtitRtf1         S            100                                                             
     DPtitRtf2         S            100                                                             
     DPtitRtf3         S            100                                                             
     DPtitRtf4         S            100                                                             
     DPtitRtf5         S            100                                                             
     DPtitRtf6         S            100                                                             
     DPVlrRtf1         S             60                                                             
     DPVlrRtf2         S             60                                                             
     DPVlrRtf3         S             60                                                             
     DPVlrRtf4         S             60                                                             
     DPVlrRtf5         S             60                                                             
     DPVlrRtf6         S             60                                                             
      *                                                                                             
     DwCedulaG         S             15S 0 Inz(*Zeros)                                              
     DwDescuento       S             11S 2 Inz(*Zeros)                                              
     DwFechaD          S             10A   Inz(*Blanks)                                             
     Dw_Region         S             10A   Inz(*Blanks)                                             
     Dw_Moneda         S              5A   Inz(*Blanks)                                             
     DTced             S             30A   Inz(*Blanks)                                             
     DTcns             S              9a   Inz(*Blanks)                                             
                                                                                                    
       //----------------------------------------------------------------//                         
       //            P r o g r a m a    P r i n c i p a l                //                         
       //----------------------------------------------------------------//                         
      /Free                                                                                         
       If pCampa <> '2009';                                                                         
        PrInicial();                                                                                
                                                                                                    
        //Cursor de Pago de Guias                                                                   
        Exec SQL                                                                                    
         Declare CursorG Cursor for                                                                 
         Select Spgced, Sum(Spgvds), Max(Spgfds)                                                    
         From Votrea00/Vttspgf                                                                      
         Where SPGCIA = :pCia                                                                       
           And SPGZON = :pZona                                                                      
           And SPGCAM = :pCampa                                                                     
           And Spgced In (Select Gldcte                                                             
                          From Votrea00/Vtmgldf                                                     
                          Where GLDCIA = :pCia                                                      
                           And (Gldest = '' Or :pCampa <= Gldcdt))                                  
        Group By Spgced                                                                             
        Order By Spgced;                                                                            
                                                                                                    
        Exec SQL                                                                                    
         Open CursorG;                                                                              
                                                                                                    
        Exec SQL                                                                                    
         Fetch CursorG Into :wCedulaG, :wDescuento, :wFechaD;                                       
                                                                                                    
        DoW Sqlcode >= 0 And Sqlcode <> 100;                                                        
            If wDescuento <> 0;                                                                     
               PrProceso();                                                                         
            EndIf;                                                                                  
            PrLimpiarVbles();                                                                       
            Exec SQL                                                                                
             Fetch CursorG Into :wCedulaG, :wDescuento, :wFechaD;                                   
        EndDo;                                                                                      
                                                                                                    
        Exec SQL                                                                                    
         Close CursorG;                                                                             
                                                                                                    
        Endif;                                                                                      
        *Inlr = *On;                                                                                
        Return;                                                                                     
                                                                                                    
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento inicial de consulta de Datos Generales                 //                   
       //----------------------------------------------------------------------//                   
     PPrInicial        B                                                                            
      *Variables locales                                                                            
     DwCia             S              3S 0 Inz(*Zeros)                                              
     DwZona            S             10A   Inz(*Blanks)                                             
      /Free                                                                                         
                                                                                                    
        Exec Sql                                                                                    
         Delete Votrea00/VTHCGTF                                                                    
         Where CGTCIA = :pCia                                                                       
           And CGTMED = :pCampa                                                                     
           And CGTZON = :pZona;                                                                     
                                                                                                    
        //Consulta codigo de Pais                                                                   
        wCia = %Int(pCia);                                                                          
        Chain (wCia) VTMPARF;                                                                       
        Pciak = PARCIN;                                                                             
                                                                                                    
        //Consulta cedula de Gerente de Zona                                                        
        wZona = pZona;                                                                              
        Chain (wZona) LSLMAN01;                                                                     
        Pidl = %Trim(SMINF1);                                                                       
        w_Region = SMINF2;                                                                          
                                                                                                    
        //Consulta simbolo de Moneda                                                                
        Chain (pCia) VTMSMPF;                                                                       
        If %Found(VTMSMPF);                                                                         
           w_Moneda = SMPSIG;                                                                       
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrInicial        E                                                                            
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que consulta Liquidación de Guía                   //                       
       //------------------------------------------------------------------//                       
     PPrProceso        B                                                                            
      *Variables Locales                                                                            
     Dw_Cedula         S             15S 0 Inz(*Zeros)                                              
     Dw_posIV          S              5U 0 Inz(*Zeros)                                              
     Dw_posFV          S              5U 0 Inz(*Zeros)                                              
     Dw_posIF          S              5U 0 Inz(*Zeros)                                              
     Dw_posFF          S              5U 0 Inz(*Zeros)                                              
     Dw_Cont           S              2S 0 Inz(*Zeros)                                              
     Dw_CamInacG       S              4A   Inz(*Blanks)                                             
     Dw_IndDatos       S               N   Inz(*Off)                                                
     Dw_ContCC         S              2S 0 Inz(*Zeros)                                              
      /Free                                                                                         
                                                                                                    
        Clear REGCGT;                                                                               
        CGTCIA = pCia;                                                                              
        CGTMED = pCampa;                                                                            
        CGTREG = w_Region;                                                                          
        CGTZON = pZona;                                                                             
        CGTCOD = wCedulaG;                                                                          
        //Consulta Nombre de Líder (Guía)                                                           
        Chain (CGTCOD) LMLLST01;                                                                    
        CGTNOM = MLNAME;                                                                            
        CGTDSC = wDescuento;                                                                        
                                                                                                    
        PidG = wCedulaG;                                                                            
                                                                                                    
        Exec Sql                                                                                    
         Delete Votrea00/Vttdetpgf;                                                                 
                                                                                                    
        GuiasLiqCampa(Pciak                                                                         
               :Pidl                                                                                
               :PidG                                                                                
               :pCampa                                                                              
               :PSec                                                                                
               :PTer                                                                                
               :Ptitcamp                                                                            
               :Ptitnamel                                                                           
               :Ptitdsopr                                                                           
               :Ptitzona1                                                                           
               :Ptitfulac                                                                           
               :Ptitmlpln                                                                           
               :Ptitpanta                                                                           
               :Ptitsecto                                                                           
               :Ptitcedgi                                                                           
               :Ptitnomgi                                                                           
               :Ptotales                                                                            
               :Ptotprest                                                                           
               :Ptotdespr                                                                           
               :Ptitbotn1                                                                           
               :Ptitbotn2                                                                           
               :Ptitbotn3                                                                           
               :Ptitbotn4                                                                           
               :Pnamelider                                                                          
               :PnameGuia                                                                           
               :Pdiasoper                                                                           
               :PzonaL                                                                              
               :Pfechaact                                                                           
               :Pmailplan                                                                           
               :PtotalSum                                                                           
               :Ptitpanta1                                                                          
               :Ptitpanta2                                                                          
               :Pstatus                                                                             
               :Pdestatus                                                                           
               :Ptitbotn5                                                                           
               :PtitCmpPto                                                                          
               :PtitActPto                                                                          
               :PCamPto                                                                             
               :PActPto                                                                             
               :PtitCmpPto1                                                                         
               :PtitActPto1                                                                         
               :PCamPtoA                                                                            
               :PActPtoA                                                                            
               :PtitTerri                                                                           
               :PtitActPtoZR                                                                        
               :PActPtoZR                                                                           
               :Ptitbotn6                                                                           
               :PtitConta                                                                           
               :PNumeroCondi                                                                        
               :PtitPagEfec                                                                         
               :PValPagEfec                                                                         
               :PtitDesActC                                                                         
               :PTotalActCre                                                                        
               :PTituFecPago                                                                        
               :PFechaPago                                                                          
               :PTitPagoDesp                                                                        
               :Ptitbotn7                                                                           
               :PtitRtf1                                                                            
               :PtitRtf2                                                                            
               :PtitRtf3                                                                            
               :PtitRtf4                                                                            
               :PtitRtf5                                                                            
               :PtitRtf6                                                                            
               :PVlrRtf1                                                                            
               :PVlrRtf2                                                                            
               :PVlrRtf3                                                                            
               :PVlrRtf4                                                                            
               :PVlrRtf5                                                                            
               :PVlrRtf6);                                                                          
                                                                                                    
        Clear ResulSetDesc;                                                                         
        Exec sql                                                                                    
         Declare CursorD Cursor For                                                                 
         Select PDESCRIP, PNUMEROL, PVALORL, PCODLIQ, PNUMCON,                                      
                SUBSTR(PTITPAGC, 1, 1), Rrn(VTTDETPGF)                                              
         From VOTREA00/VTTDETPGF                                                                    
         Where CHAR(PVALORL) <> '$0' And PTITPAGC <> ''                                             
           And PTITULO <> '*' And PCODLIQ Not In (202, 204)                                         
         Order by Rrn(VTTDETPGF);                                                                   
                                                                                                    
        Exec Sql                                                                                    
         Open CursorD;                                                                              
                                                                                                    
        Exec Sql                                                                                    
         Fetch CursorD Into :ResulSetDesc;                                                          
                                                                                                    
        If Sqlcode = *Zeros;                                                                        
           w_IndDatos = *On;                                                                        
        EndIf;                                                                                      
        DoW Sqlcode = *Zeros;                                                                       
            PrAsignaVlrs();                                                                         
            Clear ResulSetDesc;                                                                     
            Exec Sql                                                                                
             Fetch CursorD Into :ResulSetDesc;                                                      
        EndDo;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Close CursorD;                                                                             
                                                                                                    
        If w_IndDatos Or wDescuento > 0;                                                            
                                                                                                    
           //Actividad Y Crecimiento                                                                
           If PTotalActCre <> %Trim(w_Moneda);                                                      
              CGTACT = PTotalActCre;                                                                
           EndIf;                                                                                   
                                                                                                    
           //Total Descuento Plan                                                                   
           CGTTDS = PtotalSum;                                                                      
                                                                                                    
           //Ajuste en Cartera                                                                      
           If PTitPagoDesp <> *Blanks;                                                              
              CGTIAC = 'X';                                                                         
           EndIf;                                                                                   
                                                                                                    
           //Pago en Efectivo                                                                       
           If PValPagEfec <> *Blanks;                                                               
              CGTIPE = 'X';                                                                         
           EndIf;                                                                                   
                                                                                                    
           //Fecha Descuento Efectivo                                                               
           CGTFEE = PFechaPago;                                                                     
                                                                                                    
           //Valor Retefuente                                                                       
           If PVlrRtf3 <> *Blanks;                                                                  
              CGTVRT = %Trim(w_Moneda)+ ' ' +%Trim(PVlrRtf3);                                       
           EndIf;                                                                                   
        EndIf;                                                                                      
                                                                                                    
        //Se obtiene la Cedula de la guia mediante la rutina VTMCSBCR                               
        If wCedulaG >= 995000000;                                                                   
           Clear Tced;                                                                              
           Tcns = %Trim(%Char(wCedulaG));                                                           
           ConversionID(pCia:Tced:Tcns);                                                            
           Monitor;                                                                                 
            w_Cedula = %Int(%Trim(Tced));                                                           
           On-Error;                                                                                
            w_Cedula = wCedulaG;                                                                    
           EndMon;                                                                                  
        Else;                                                                                       
           w_Cedula = wCedulaG;                                                                     
        EndIf;                                                                                      
                                                                                                    
                                                                                                    
        //Consulta Si Guia esta retenida por Rut                                                    
        If pCia = '001';                                                                            
           Chain (pCia:w_Cedula) VTMRTGF;                                                           
           If Not %Found(VTMRTGF);                                                                  
              CGTIRR = 'X';                                                                         
           Else;                                                                                    
              If RTGEST = 'D';                                                                      
                 CGTIRR = 'X';                                                                      
                 If RTGCDT <> '';                                                                   
                    Exec Sql                                                                        
                     Select GLDCDT Into :w_CamInacG                                                 
                     From Votrea00/VTMGLDF                                                          
                     Where GLDCIA = :pCia                                                           
                       And GLDCTE = :wCedulaG                                                       
                       And GLDEST = 'D'                                                             
                       And GLDZON = :Pzona                                                          
                     Order By RRN(VTMGLDF) Desc;                                                    
                    If w_CamInacG = RTGCDT And w_CamInacG >= pCampa;                                
                       Clear CGTIRR;                                                                
                    EndIf;                                                                          
                 EndIf;                                                                             
              EndIf;                                                                                
           EndIf;                                                                                   
        EndIf;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Select Count(1) Into :w_ContCC                                                             
         From Votrea00/VTMIPGF                                                                      
         Where IPGCIA = :pCia                                                                       
           And IPGCGC = :w_Cedula                                                                   
           And IPGEST In ('', 'A')                                                                  
           And IPGOBS = 'Cuenta Creada Correctamente';                                              
                                                                                                    
        If w_ContCC > 0;                                                                            
           CGTICC = 'X';                                                                            
        EndIf;                                                                                      
                                                                                                    
        Write(E) REGCGT;                                                                            
                                                                                                    
      /End-Free                                                                                     
     PPrProceso        E                                                                            
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento que asigna valores de SP GSL_GuiasDetLiquid            //                   
       //----------------------------------------------------------------------//                   
     PPrAsignaVlrs     B                                                                            
      /Free                                                                                         
                                                                                                    
        If ResulSetDesc.PTitPagC = 'O';        //Oro                                                
           Select;                                                                                  
            When ResulSetDesc.PCodLiq = 1;                                                          
               CGTNP1O = ResulSetDesc.PnumeroL;                                                     
               CGTVP1O = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 50;                                                         
               CGTNF1O = ResulSetDesc.PnumeroL;                                                     
               CGTVF1O = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 2;                                                          
               CGTNP2O = ResulSetDesc.PnumeroL;                                                     
               CGTVP2O = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 3;                                                          
               CGTNP3O = ResulSetDesc.PnumeroL;                                                     
               CGTVP3O = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 210;                                                        
               CGTNFNO = ResulSetDesc.PnumeroL;                                                     
               CGTVFNO = ResulSetDesc.PvalorL;                                                      
           EndSl;                                                                                   
        ElseIf ResulSetDesc.PTitPagC = 'P';    //Plata                                              
           Select;                                                                                  
            When ResulSetDesc.PCodLiq = 1;                                                          
               CGTNP1P = ResulSetDesc.PnumeroL;                                                     
               CGTVP1P = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 50;                                                         
               CGTNF1P = ResulSetDesc.PnumeroL;                                                     
               CGTVF1P = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 2;                                                          
               CGTNP2P = ResulSetDesc.PnumeroL;                                                     
               CGTVP2P = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 3;                                                          
               CGTNP3P = ResulSetDesc.PnumeroL;                                                     
               CGTVP3P = ResulSetDesc.PvalorL;                                                      
            When ResulSetDesc.PCodLiq = 210;                                                        
               CGTNFNP = ResulSetDesc.PnumeroL;                                                     
               CGTVFNP = ResulSetDesc.PvalorL;                                                      
           EndSl;                                                                                   
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPrAsignaVlrs     E                                                                            
                                                                                                    
                                                                                                    
       //----------------------------------------------------------------------//                   
       // Procedimiento que limpia variables de Trabajo                        //                   
       //----------------------------------------------------------------------//                   
     PPrLimpiarVbles   B                                                                            
      /Free                                                                                         
                                                                                                    
        Clear wCedulaG;                                                                             
        Clear wDescuento;                                                                           
        Clear wFechaD;                                                                              
        Clear PTotalActCre;                                                                         
        Clear PtotalSum;                                                                            
        Clear PTitPagoDesp;                                                                         
        Clear PValPagEfec;                                                                          
        Clear PFechaPago;                                                                           
                                                                                                    
      /End-Free                                                                                     
     PPrLimpiarVbles   E                                                                            
