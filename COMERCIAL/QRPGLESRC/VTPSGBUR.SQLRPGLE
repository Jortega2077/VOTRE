      *********************************************************************                         
      * Programa      : VTPSGBUR                                          *                         
      * Descripción:  : Envio de pagos de Descuentos (DAVIPLATA)          *                         
      * Proyecto      : Plan Guías                                        *                         
      * Autor         : (AParraQ)                                         *                         
      * Fecha Creación: Enero de 2020                                     *                         
      *-------------------------------------------------------------------*                         
      *                                                                   *                         
      * CRTPGM PGM(VOTREP00/VTPSGBUR) MODULE(VOTREP00/VTPSGBUR)           *                         
      * ACTGRP(*CALLER)                                                   *                         
      *                                                                   *                         
      *********************************************************************                         
      * Definición de Directivas de Compilación                                                     
       CTL-OPT Option(*NoDebugIO);                                                                  
       CTL-OPT Datfmt(*DMY);                                                                        
                                                                                                    
      *Definición de Archivos                                                                       
       DCL-F VTWPGEF DISK USAGE(*INPUT:*UPDATE) KEYED;                                              
       DCL-F VTMBNSF DISK USAGE(*INPUT) KEYED;                                                      
                                                                                                    
      *Parametro de Entrada al Programa (Entry)                                                     
       DCL-PR MAIN EXTPGM('VTPSGBUR');                                                              
         pCia CHAR(3) CONST;                                                                        
         pFile CHAR(11) CONST;                                                                      
       END-PR;                                                                                      
                                                                                                    
       DCL-PI MAIN;                                                                                 
         pCia CHAR(3) CONST;                                                                        
         pFile CHAR(11) CONST;                                                                      
       END-PI;                                                                                      
                                                                                                    
      * Definición estructura del sistema                                                           
       DCL-DS AS400 PSDS;                                                                           
         Nom_Pgm   *PROC;                                                                           
         Libreria CHAR(10) POS(81);                                                                 
         Nom_Job CHAR(10) POS(244);                                                                 
         Usuario CHAR(10) POS(254);                                                                 
         Nro_Job CHAR(10) POS(264);                                                                 
         Sig_sys ZONED(2) POS(195);                                                                 
         AÑo_sys ZONED(2) POS(280);                                                                 
         Mes_sys ZONED(2) POS(276);                                                                 
         Dia_sys ZONED(2) POS(278);                                                                 
       END-DS;                                                                                      
                                                                                                    
      * Definición estructura de Control (Encabezado)                                               
       DCL-DS EDAVIPLATA;                                                                           
        EIni CHAR(2) INZ('RC');                                                                     
        ENit ZONED(16) INZ(8001001593);                                                             
        ECodSer CHAR(4) INZ(*Zeros);                                                                
        ECodSubSer CHAR(4) INZ(*Zeros);                                                             
        ECuentaE CHAR(16) INZ('0560036269998716');                                                  
        ETipoCta CHAR(2) INZ('CC');                                                                 
        ECodBanco CHAR(6);  //000051                                                                
        EValorT ZONED(18:2);                                                                        
        ENroT ZONED(6);                                                                             
        EFechaP CHAR(8);  //AAAMMDD                                                                 
        EHoraP CHAR(6);   //HHMMSS                                                                  
        ECodOper CHAR(4) INZ(*Zeros);                                                               
        ECodNoProc CHAR(4) INZ('9999');                                                             
        EFechaG CHAR(8) INZ(*Zeros);                                                                
        EHoraG CHAR(6) INZ(*Zeros);                                                                 
        EIndInsc CHAR(2) INZ(*Zeros);                                                               
        ETipoId CHAR(2) INZ('01');                                                                  
        ENroCliente CHAR(12) INZ(*Zeros);                                                           
        EOfRecaudo CHAR(4) INZ(*Zeros);                                                             
        ECampo CHAR(40) INZ(*Zeros);                                                                
        EDatosR CHAR(170) POS(1);                                                                   
       END-DS;                                                                                      
                                                                                                    
      * Definición estructura de Traslado (Detalle)                                                 
       DCL-DS DDAVIPLATA DIM(9999) QUALIFIED;                                                       
        DIni CHAR(2) INZ('TR');                                                                     
        DNit ZONED(16);                                                                             
        DReferencia CHAR(16) INZ(*Zeros);                                                           
        DProducto ZONED(16);                                                                        
        DTipoProd CHAR(2) INZ('DP');  //03-DP: Daviplata                                            
        DCodBanco CHAR(6);   //000051: Davivienda                                                   
        DValor ZONED(18:2);                                                                         
        DTalon CHAR(6) INZ(*Zeros);                                                                 
        DTipoNit CHAR(2);                                                                           
        DACH CHAR(1) INZ('1');                                                                      
        DResult CHAR(4) INZ('9999');                                                                
        DRespuesta CHAR(40) INZ(*Zeros);                                                            
        DValorAC ZONED(18:2) INZ(*Zeros);                                                           
        DFechaAP CHAR(8) INZ(*Zeros);                                                               
        DOficinaR CHAR(4) INZ(*Zeros);                                                              
        DMotivo CHAR(4) INZ(*Zeros);                                                                
        DCampo CHAR(7) INZ(*Zeros);                                                                 
        DDatosR CHAR(170) POS(1);                                                                   
       END-DS;                                                                                      
                                                                                                    
      * Programa de FTP                                                                             
       DCL-PR PgmFtp EXTPGM('VOTREP00/VTMGOI2R');                                                   
         PindIFS CHAR(1) CONST;                                                                     
         PcodApli PACKED(3:0) CONST;                                                                
         PdesPli CHAR(2) CONST;                                                                     
         Pextension CHAR(10) CONST;                                                                 
         ParchivoOri CHAR(100) CONST;                                                               
         ParchivoDes CHAR(100) CONST;                                                               
         Plibre CHAR(200) CONST;                                                                    
       END-PR;                                                                                      
                                                                                                    
      *Definicion de Variables de Trabajo                                                           
       DCL-S w_Banco CHAR(4) INZ('051');                                                            
       DCL-S Idx ZONED(4) INZ(0);                                                                   
       DCL-S PindIFS CHAR(1);                                                                       
       DCL-S PcodApli PACKED(3:0);                                                                  
       DCL-S PdesPli CHAR(2);                                                                       
       DCL-S Pextension CHAR(10);                                                                   
       DCL-S ParchivoOri CHAR(100);                                                                 
       DCL-S ParchivoDes CHAR(100);                                                                 
       DCL-S Plibre CHAR(200);                                                                      
      *DCL-S w_CodBanco CHAR(6) INZ(*Blanks);                                                       
                                                                                                    
       //----------------------------------------------------------------//                         
       //            P r o g r a m a    P r i n c i p a l                //                         
       //----------------------------------------------------------------//                         
      /Free                                                                                         
                                                                                                    
                                                                                                    
       Chain (pCia:w_Banco) VTMBNSF;                                                                
       If %Found(VTMBNSF);                                                                          
          ECodBanco = BNSCCB;                                                                       
       EndIf;                                                                                       
                                                                                                    
       Clear EValorT;                                                                               
       Clear ENroT;                                                                                 
       SetLl *Loval VTWPGEF;                                                                        
       Read VTWPGEF;                                                                                
       If Not %Eof(VTWPGEF);                                                                        
          PrCreateFile();                                                                           
       EndIf;                                                                                       
       Dow Not %Eof(VTWPGEF);                                                                       
         If PGEEST = 'P' And PGECDB = w_Banco;                                                      
            //Estructura Detalle                                                                    
            Idx += 1;                                                                               
                                                                                                    
            DDAVIPLATA(Idx).DNit = PGECDG;                                                          
            DDAVIPLATA(Idx).DProducto = %Int(PGENRC);                                               
            DDAVIPLATA(Idx).DCodBanco = ECodBanco;                                                  
            DDAVIPLATA(Idx).DValor = PGEVLP;                                                        
            DDAVIPLATA(Idx).DTipoNit = '02';  //02: Cédula de Ciudadanía                            
                                                                                                    
            //Estructura Encabezado                                                                 
            EValorT += PGEVLP;                                                                      
            ENroT += 1;                                                                             
                                                                                                    
            PGEEST = 'V';                                                                           
            PGECPG = PrMedia();                                                                     
            If PGECPG = '2009';                                                                     
             PGECPG = '2008';                                                                       
            Endif;                                                                                  
            Update REGPGE %Fields(PGEEST:PGECPG);                                                   
         EndIf;                                                                                     
         Read VTWPGEF;                                                                              
       EndDo;                                                                                       
                                                                                                    
       If Idx > 0;                                                                                  
          EFechaP = %Char(%Date():*Iso0);                                                           
          EHoraP = %char(%time():*Iso0);                                                            
          Exec Sql                                                                                  
           Insert into QTEMP.DAVIPLATAF values (:EDatosR);                                          
          If SqlCode = 0;                                                                           
             PrDetalle();                                                                           
             //PrFTP();                                                                             
          EndIf;                                                                                    
       EndIf;                                                                                       
                                                                                                    
       *Inlr = *On;                                                                                 
       Return;                                                                                      
                                                                                                    
      /End-Free                                                                                     
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta la Campaña Actual                     //                       
       //------------------------------------------------------------------//                       
       DCL-PROC PrMedia;                                                                            
        DCL-PI PrMedia CHAR(4);                                                                     
        END-PI;                                                                                     
      *Variables de trabajo                                                                         
        DCL-S w_Media CHAR(4) INZ(*Blanks);                                                         
                                                                                                    
        Exec Sql                                                                                    
         Select EstCam Into :w_Media                                                                
         From Orderp00/Opiestf1                                                                     
         Where EstCia = :pCia                                                                       
           And EstZon = :PGEZNG                                                                     
           And EstEst = 'S'                                                                         
         Order by Rrn(Opiestf1) Desc                                                                
         Fetch First 1 Row Only;                                                                    
                                                                                                    
        Return w_Media;                                                                             
                                                                                                    
       END-PROC;                                                                                    
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea archivo temporal                          //                       
       //------------------------------------------------------------------//                       
       DCL-PROC PrCreateFile;                                                                       
                                                                                                    
        Exec Sql                                                                                    
         Drop Table QTEMP.DAVIPLATAF;                                                               
                                                                                                    
        Exec Sql                                                                                    
         Create Table QTEMP.DAVIPLATAF (                                                            
          TRAMA CHAR(170) CCSID 284 NOT NULL);                                                      
                                                                                                    
       END-PROC;                                                                                    
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Inserta registros de Detalle                   //                       
       //------------------------------------------------------------------//                       
       DCL-PROC PrDetalle;                                                                          
      *Variables de trabajo                                                                         
        DCL-S I ZONED(4) INZ(0);                                                                    
        DCL-S RtramaD CHAR(170) INZ(*Blanks);                                                       
                                                                                                    
        For I = 1 To Idx;                                                                           
           RtramaD = DDAVIPLATA(I).DDatosR;                                                         
           Exec Sql                                                                                 
            Insert into QTEMP.DAVIPLATAF values (:RtramaD);                                         
        EndFor;                                                                                     
                                                                                                    
       END-PROC;                                                                                    
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que invoca pgm para FTP                            //                       
       //------------------------------------------------------------------//                       
       DCL-PROC PrFTP;                                                                              
                                                                                                    
        PindIFS = 'N';                                                                              
        PcodApli = 1;                                                                               
        PdesPli = 'NR';                                                                             
        Plibre = 'QTEMP';                                                                           
        Pextension = 'TXT';                                                                         
        ParchivoOri = 'DAVIPLATAF';                                                                 
        ParchivoDes = 'PD' + pFile;                                                                 
                                                                                                    
        PgmFtp(PindIFS                                                                              
               :PcodApli                                                                            
               :PdesPli                                                                             
               :Pextension                                                                          
               :ParchivoOri                                                                         
               :ParchivoDes                                                                         
               :Plibre);                                                                            
                                                                                                    
       END-PROC;                                                                                    
