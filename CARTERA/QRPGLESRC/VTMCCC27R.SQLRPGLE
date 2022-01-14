     ******************************************************W***************                         
     ** Programa      : VTMCCC27R                                        **                         
     ** Descripciòn   : S.P. Mensaje en Informe Estudio de               **                         
     **                 Reincorporación                                  **                         
     ** Proyecto      :                                                  **                         
     ** Store Procedure: S.P. PDL_ValidEstadoCuenta                      **                         
     ** Autor         : Leonel Mauricio Parra Suárez-PersonalSoft        **                         
     ** Fecha Creaciòn: 26 de Septiembre de 2017                         **                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HDftactgrp(*no) Option(*Srcstmt)                                                               
     HDatfmt(*Ymd)                                                                                  
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pr                  Extpgm('VTMCCC27R')                                      
     Dspcia                          10a                                                            
     DPcodg                          15  0                                                          
     DPnroPed                         3  0                                                          
     DPsaldo                         15a                                                            
     DPfpago                         30a                                                            
     DSpsts                           1a                                                            
     DSpdes                         350a                                                            
     DMain             Pi                                                                           
     Dspcia                          10a                                                            
     DPcodg                          15  0                                                          
     DPnroPed                         3  0                                                          
     DPsaldo                         15a                                                            
     DPfpago                         30a                                                            
     DSpsts                           1a                                                            
     DSpdes                         350a                                                            
                                                                                                    
      * Definición de Variables de Trabajo                                                          
     D$COMA            C                   ''''                                                     
     DCompr            S             11                                                             
     DTcia             s              3                                                             
     DTced             s             30                                                             
     DTcns             s              9                                                             
     D$Prms            S              1                                                             
     D$Lbrs            S             10    Inz(' ')                                                 
     D$Optn            S             10    Inz('*FIRST')                                            
     D$Refe            S             10    Inz(' ')                                                 
     DWcia             s              3a   Inz(*Blanks)                                             
     DKRes             s              2a   Inz(*Blanks)                                             
     DKCiaR            s             10a   Inz(*Blanks)                                             
     Dw_SpCedCorta     s              9  0                                                          
     DrsDemog          S                   SQLTYPE(RESULT_SET_LOCATOR)                              
                                                                                                    
      *                                                                                             
     Ddsresult         Ds                  Qualified Dim(50)                                        
     DCPedido                         4                                                             
     dCOrder                          9  0                                                          
     dEped                            1                                                             
     dVPedido                        15                                                             
     dVasignado                      15                                                             
     dVFacturado                     15                                                             
     dEPedidob                       80                                                             
     dURL                          1000                                                             
     dTipoReg                         1                                                             
     dXML                         30000                                                             
      *estructuras de trabajo                                                                       
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
      * Definición de Variables de Trabajo                                                          
     ** Area de Mensajes a Enviarle a las Pantallas en Java                                         
     DMsg1             C                   'No se Enviaron los Parametros +                         
     D                                      Requeridos'                                             
                                                                                                    
     ** Definición de Procedimientos de Trabajo                                                     
     DLimpiarVbles     Pr                                                                           
     DLibrerias        Pr                                                                           
     DPrTitulos        Pr                                                                           
     DPrConsultarPlan  Pr                                                                           
      *                                                                                             
     DPrCompaÑia       Pr                  Extpgm('VOTREP00/VTMCSBFR')                              
     DSPcia                          10a                                                            
     DKRes                            2a                                                            
     DKCiaR                          10a                                                            
                                                                                                    
     DLibreria         Pr                  Extpgm('VOTREP00/VTMGOB2R')                              
     D$Prms                           1                                                             
     D$Lbrs                          10                                                             
     D$Optn                          10                                                             
     D$Refe                          10                                                             
     DConversionID     Pr                  Extpgm('VOTREP00/VTMCSBCR')                              
     DTcia                            3                                                             
     DTced                           30                                                             
     DTcns                            9                                                             
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
        //Valida el Número de Parametros Enviados desde Java                                        
        If %Parms < 6;                                                                              
           SpSts   = '1';                                                                           
           spDes     = Msg1;                                                                        
           *Inlr = *On;                                                                             
           Return;                                                                                  
        EndIf;                                                                                      
        LimpiarVbles();                                                                             
        //Se Valida Existencia de Compañia                                                         
        CallP PrCompaÑia(SPcia:KRes:KCiaR);                                                         
        WCia= %Trim(KCiaR);                                                                         
        If KRes= 'No';                                                                              
           SpSts = '1';                                                                             
           SpDes = 'La compañia no existe en el sistema';                                           
           Return;                                                                                  
        ElseIf Pcodg <=  *Zeros;                                                                    
           SpSts = '1';                                                                             
           SpDes = 'Ingrese Cédula de Compradora';                                                  
           Return;                                                                                  
        EndIf;                                                                                      
        If SpSts   = *Blanks;                                                                       
           $Prms = 'A';                                                                             
           Librerias();                                                                             
           Pr_Principal();                                                                          
           $Prms = 'R';                                                                             
           Librerias();                                                                             
        EndIf;                                                                                      
        If SpSts   = *Blanks;                                                                       
           spSts   = '0';                                                                           
           SpDes     = 'Ok, Información enviada con éxito';                                         
        EndIf;                                                                                      
        *Inlr = *On;                                                                                
        Return;                                                                                     
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //----------------------------------------------------------------//                         
       //  Subprocedimiento que Limpia parametros de salida              //                         
       //----------------------------------------------------------------//                         
     PLimpiarVbles     B                                                                            
     DLimpiarVbles     Pi                                                                           
      /Free                                                                                         
        Clear Psaldo ;                                                                              
        Clear Pfpago ;                                                                              
        Clear Spsts  ;                                                                              
        Clear Spdes  ;                                                                              
      /End-Free                                                                                     
     PLimpiarVbles     E                                                                            
                                                                                                    
       //----------------------------------------------------------------//                         
       //  Subprocedimiento que Adiciona Librerias de Trabajo            //                         
       //----------------------------------------------------------------//                         
     PLibrerias        B                                                                            
     DLibrerias        Pi                                                                           
      /Free                                                                                         
                                                                                                    
        // Adiciona o Remueve Librerias de Trabajo                                                  
        Clear $Lbrs;                                                                                
        $Lbrs = 'VOTREA' + %Trim(WCia );                                                            
        Callp Libreria($Prms:$Lbrs:$Optn:$Refe);                                                    
                                                                                                    
        Clear $Lbrs;                                                                                
        $Lbrs = 'VOTREA00';                                                                         
        Callp Libreria($Prms:$Lbrs:$Optn:$Refe);                                                    
                                                                                                    
        Clear $Lbrs;                                                                                
        $Lbrs = 'OPF' + %Trim(WCia );                                                               
        Callp Libreria($Prms:$Lbrs:$Optn:$Refe);                                                    
                                                                                                    
      /End-Free                                                                                     
     PLibrerias        E                                                                            
                                                                                                    
       //----------------------------------------------------------------//                         
       //  Subprocedimiento que Adiciona Librerias de Trabajo            //                         
       //----------------------------------------------------------------//                         
     PPr_Principal     B                                                                            
     DPr_Principal     Pi                                                                           
     Dw_MLDM01         s              1    Inz(*Blanks)                                             
     Dw_Mldm03         s              1    Inz(*Blanks)                                             
     Dw_Spcia          s             10    Inz(*Blanks)                                             
     Dw_Spced          s             15  0 Inz(*Zeros)                                              
     Dw_Spvacon        s             15    Inz(*Blanks)                                             
     Dw_Spfpago        s             30    Inz(*Blanks)                                             
     Dw_Spcupn         s              9  0 Inz(*Zeros)                                              
     Dw_Spcupo         s             15    Inz(*Blanks)                                             
     Dw_Spsts          s              1    Inz(*Blanks)                                             
     Dw_Spdes          s            350    Inz(*Blanks)                                             
     DStMt             s            350    Inz(*Blanks)                                             
      /Free                                                                                         
       Clear Tced;                                                                                  
       Clear Tcns;                                                                                  
       Tced = %Char(Pcodg);                                                                         
       ConversionID(WCia:Tced:Tcns);                                                                
       w_SpCedCorta = %Int(Tcns);                                                                   
                                                                                                    
        Exec Sql                                                                                    
        Select MLDM01, MLDM03 into :w_MLDM01,:w_MLDM03                                              
        from  LMLLST01                                                                              
        where MLCSTÑ = :w_SpCedCorta;                                                               
        If SqlCod = *Zeros;                                                                         
           If w_MLDM01= 'X' And w_MLDM03 = '1';                                                     
              w_Spcia = spcia;                                                                      
              w_Spced = Pcodg;                                                                      
              StMt = 'Call Votrep00/CONSULTORDERN(?,?,?,?,?,?,?,?)';                                
                                                                                                    
              Exec Sql                                                                              
               Prepare PreStmt From :StMt;                                                          
              Exec Sql                                                                              
               Execute PreStmt Using :w_Spcia ,                                                     
                                     :w_Spced ,                                                     
                                     :w_Spvacon ,                                                   
                                     :w_Spfpago ,                                                   
                                     :w_Spcupn ,                                                    
                                     :w_Spcupo ,                                                    
                                     :w_Spsts ,                                                     
                                     :w_Spdes;                                                      
              Exec SQL                                                                              
                ASSOCIATE RESULT SET LOCATORS                                                       
                (:RsDemog)                                                                          
                WITH PROCEDURE Votrep00/CONSULTORDERN;                                              
                                                                                                    
              Exec SQL                                                                              
                ALLOCATE CuDemogra CURSOR                                                           
                FOR RESULT SET :RsDemog;                                                            
                                                                                                    
              Exec SQL                                                                              
                FETCH CuDemogra FOR 1 ROWS INTO :dsresult;                                          
                                                                                                    
              Exec SQL                                                                              
                CLOSE CuDemogra;                                                                    
                                                                                                    
               Compr = 'Compradora ';                                                               
               If Wcia  = '001' or Wcia = '002';                                                    
                  Compr = 'Ejecutiva';                                                              
               Else;                                                                                
                  If Wcia  = '200';                                                                 
                     Compr = 'Consultora ';                                                         
                  Else;                                                                             
                     If Wcia = '450';                                                               
                        Compr = 'Consejera ';                                                       
                     Endif;                                                                         
                  Endif;                                                                            
               Endif;                                                                               
                                                                                                    
                 spSts = '1';                                                                       
                 Spdes  = 'La ' + Compr + 'tiene un saldo vencido de '                              
                     + %Trim(w_Spvacon) +                                                           
                    ',  para acceder al beneficio que le ofrece el'+                                
                    ' estudio de Reincorporación debe estar al día';                                
           EndIf;                                                                                   
        ElseIf SqlCod <> *Zeros;                                                                    
           spSts = '1';                                                                             
           SpDes = 'Cédula de Ejecutiva: "'+%Trim(%Editc(Pcodg:'P'))                                
                   +'", no existe';                                                                 
        EndIf;                                                                                      
                                                                                                    
      /End-Free                                                                                     
     PPr_Principal     E                                                                            
                                                                                                    
