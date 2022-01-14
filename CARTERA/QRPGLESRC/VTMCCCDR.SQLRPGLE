      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCDR                                          **                        
      * Descripcion   : Consulta de Saldos Generales Por Copradora        **                        
      * Proyecto      : Menu Opciones O.P.I Paises                        **                        
      * Autor         : Dbetancourt                                       **                        
      * Fecha creacion: Febrero 24 del 2.009                              **                        
      *                                                                   *                         
      * Modificación  :                                                   *                         
      * Autor         : Satehortua y Dbetancourt                          **                        
      * Fecha creacion: Mayo 7 del 2.014                              **                            
      * Descripcion   : Agregar el  Saldos Generales Por Copradora        **                        
      *                                                                   *                         
      * Modificación  :                                                   *                         
      * Autor         : John William Palacio Cardenas PersonalSoft        **                        
      * Fecha creacion: Octubre 27 de 2014                            **                            
      * Descripcion   : Se agrega el campo WILKAP                         **                        
      *                                                                   *                         
      *********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
     FLardtl01  If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcpci0p  Cf   e             Workstn Include(Rdato2)                                          
                                                                                                    
     DBALCAR           S             15S 2 INZ(*Zeros)                                              
     DFECCOM           S             10A                                                            
     D AdcstÑ          S              9s 0 INZ(*Zeros)                                              
     D SqlNr1          S           2500A   INZ(*BLANKS)                                             
     D SqlNr2          S           2500A   INZ(*BLANKS)                                             
     D OSIGLO          S              2A   INZ(*BLANKS)                                             
     D OAÑO            S              2A   INZ(*BLANKS)                                             
     D OMES            S              2A   INZ(*BLANKS)                                             
     D ODIA            S              2A   INZ(*BLANKS)                                             
     DW_TOTKAP         S             15S 2 INZ(*BLANKS)                                             
     DW_indProc        S              1A   INZ('S')                                                 
     D VLRPEND         s             14  2                                                          
                                                                                                    
                                                                                                    
     DFields           Ds                                                                           
     DTCoiCoh                         3  0                                                          
     DTCoiCod                         3  0                                                          
     DTCoiCoi                         3  0                                                          
     DTCoiCpi                         3  0                                                          
     DTCoiCpd                         3  0                                                          
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DPrSaldos         Pr                                                                           
      *********************************************************************                         
     C     *ENTRY        PLIST                                                                      
     C                   PARM                    CIAO              3                                
                                                                                                    
      /FREE                                                                                         
       SqlNr2 = 'Select CoiCoh, CoiCod, CoiCoi, CoiCpi, CoiCpd ' +                                  
                'From VOTREA00/VTMCOIF Where CoiCia = ' + Ciao;                                     
                                                                                                    
       Exec Sql                                                                                     
         Prepare C2 From :SqlNr2;                                                                   
                                                                                                    
       Exec Sql                                                                                     
         Declare C2 Scroll Cursor For C2;                                                           
                                                                                                    
       Exec Sql                                                                                     
         Open C2;                                                                                   
                                                                                                    
       Exec Sql                                                                                     
         fetch C2 Into :Fields;                                                                     
                                                                                                    
       If Sqlcode <> *Zeros;                                                                        
         W_indProc = 'N';                                                                           
       EndIf;                                                                                       
         Exec Sql                                                                                   
          Close C2;                                                                                 
                                                                                                    
       //*La siguiente Instruccion SQL se crea la tabla donde se van a llevar                       
       //*Los datos de la Consulta                                                                  
         SqlNr2 = 'Create Table Votrea'+ ciao + '/vtwi1bf (' +                                      
         'WI1CED NUMERIC(9), WI1SDO NUMERIC(15, 2), WI1FEC CHAR(10), ' +                            
         'WI1VAL Numeric(15, 2))' ;                                                                 
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur1 From :SqlNr2;                                                              
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur1;                                                                          
                                                                                                    
          SqlNr2 = *Blanks;                                                                         
          SqlNr2 = 'Delete From Votrea'+ ciao + '/vtwi1bf';                                         
           Exec Sql                                                                                 
            Prepare Cur2 From :SqlNr2;                                                              
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur2;                                                                          
                                                                                                    
         Exec Sql                                                                                   
           Declare C1 Cursor For                                                                    
           Select AdcstÑ                                                                            
           From lardtl01                                                                            
           Group By AdcstÑ;                                                                         
                                                                                                    
         Exec Sql                                                                                   
          Open C1;                                                                                  
                                                                                                    
         Exec Sql                                                                                   
          Fetch C1 Into :AdcstÑ;                                                                    
                                                                                                    
         Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                    
          PrSaldos();                                                                               
           FECCOM = ODIA + '/' + OMES + '/' + OSIGLO + OAÑO;                                        
             If Ciao = '400' ;                                                                      
                VLRPEND  = 0;                                                                       
                Exec SQL                                                                            
                 SELECT IfNull(sum(dcivlr), 0) into : VLRPEND                                       
                 FROM vtedcif                                                                       
                 WHERE  DCIECO = ' '  AND                                                           
                 dcicod= :AdcstÑ;                                                                   
                 W_TotKap = BALCAR - VLRPEND;                                                       
             Else;                                                                                  
              W_TotKap = BALCAR - W_TotKap;                                                         
             EndIf;                                                                                 
                                                                                                    
           Exec Sql                                                                                 
            Insert Into Vtwi1bf(Wi1ced, Wi1sdo, Wi1fec, WI1VAL)                                     
                        Values(:AdcstÑ, :BALCAR, :FECCOM, :W_TotKap);                               
         Exec Sql                                                                                   
          Fetch C1 Into :AdcstÑ;                                                                    
         Enddo;                                                                                     
                                                                                                    
         Exec Sql                                                                                   
          Close C1;                                                                                 
                                                                                                    
         // Crea Tabla  vtwilbf  omite los WEB, AMA, DAF,                                           
         // DLM, COR, RCD, RIP, MLI                                                                 
         SqlNr1 = *Blanks;                                                                          
         SqlNr1 = 'Create Table Votrea'+ ciao + '/vtwilbf (' +                                      
        'WILZON CHAR(10), -                                                                         
        WILCED NUMERIC(9), WILRUC NUMERIC(15), WILRUA CHAR(30), -                                   
        WILNOM CHAR(30), WILCUS CHAR(3), -                                                          
        WILDIR CHAR(180), WILCTY CHAR(20), WILTL1 NUMERIC(15), -                                    
        WILTL2 NUMERIC(15), WILFEC CHAR(10), WILVAL NUMERIC(14, 2), -                               
        WILKAP NUMERIC(15, 2), WILMDO CHAR(10), WILMDL CHAR(10), -                                  
        WILCRD NUMERIC(9), WILINF CHAR(10), WILDEL CHAR(1), -                                       
        WILMLD CHAR(1), WILDI2 CHAR(180), WILBRD CHAR(30), -                                        
        WILD02 CHAR(1), WILD03 CHAR(1), WILD06 CHAR(1), -                                           
        WILD09 CHAR(1), WILD14 CHAR(1), WILD18 CHAR(1), WCUSTN CHAR(3), -                           
        WILDI1 CHAR(120), WILCI1 CHAR(60), WILZI1 CHAR(60))';                                       
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur From :SqlNr1;                                                               
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur;                                                                           
                                                                                                    
                                                                                                    
          SqlNr1 = *Blanks;                                                                         
          SqlNr1 = 'Delete From Votrea'+ ciao + '/vtwilbf';                                         
           Exec Sql                                                                                 
            Prepare Cur1 From :SqlNr1;                                                              
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur1;                                                                          
                                                                                                    
         SqlNr1 = *Blanks;                                                                          
         // Crea Tabla  vtwilbfw   para guardar solo los WEB                                        
         SqlNr1 = 'Create Table Votrea'+ ciao + '/vtwilbfW (' +                                     
        'WILZON CHAR(10), -                                                                         
        WILCED NUMERIC(9), WILRUC NUMERIC(15), WILRUA CHAR(30), -                                   
        WILNOM CHAR(30), WILCUS CHAR(3), -                                                          
        WILDIR CHAR(180), WILCTY CHAR(20), WILTL1 NUMERIC(15), -                                    
        WILTL2 NUMERIC(15), WILFEC CHAR(10), WILVAL NUMERIC(14, 2), -                               
        WILKAP NUMERIC(15, 2), WILMDO CHAR(10), WILMDL CHAR(10), -                                  
        WILCRD NUMERIC(9), WILINF CHAR(10), WILDEL CHAR(1), -                                       
        WILMLD CHAR(1), WILDI2 CHAR(180), WILBRD CHAR(30), -                                        
        WILD02 CHAR(1), WILD03 CHAR(1), WILD06 CHAR(1), -                                           
        WILD09 CHAR(1), WILD14 CHAR(1), WILD18 CHAR(1), WCUSTN CHAR(3), -                           
        WILDI1 CHAR(120), WILCI1 CHAR(60), WILZI1 CHAR(60))';                                       
                                                                                                    
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur From :SqlNr1;                                                               
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur;                                                                           
                                                                                                    
          SqlNr1 = *Blanks;                                                                         
          SqlNr1 = 'Delete From Votrea'+ ciao + '/vtwilbfW';                                        
           Exec Sql                                                                                 
            Prepare Cur1 From :SqlNr1;                                                              
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur1;                                                                          
                                                                                                    
                                                                                                    
         SqlNr1 = *Blanks;                                                                          
         // Crea Tabla  vtwilbfA   para guardar AMA...                                              
         // AMA, DAF, DLM, COR, RCD, RIP, MLI                                                       
                                                                                                    
         SqlNr1 = 'Create Table Votrea'+ ciao + '/vtwilbfA (' +                                     
        'WILZON CHAR(10), -                                                                         
        WILCED NUMERIC(9), WILRUC NUMERIC(15), WILRUA CHAR(30), -                                   
        WILNOM CHAR(30), WILCUS CHAR(3), -                                                          
        WILDIR CHAR(180), WILCTY CHAR(20), WILTL1 NUMERIC(15), -                                    
        WILTL2 NUMERIC(15), WILFEC CHAR(10), WILVAL NUMERIC(14, 2), -                               
        WILKAP NUMERIC(15, 2), WILMDO CHAR(10), WILMDL CHAR(10), -                                  
        WILCRD NUMERIC(9), WILINF CHAR(10), WILDEL CHAR(1), -                                       
        WILMLD CHAR(1), WILDI2 CHAR(180), WILBRD CHAR(30), -                                        
        WILD02 CHAR(1), WILD03 CHAR(1), WILD06 CHAR(1), -                                           
        WILD09 CHAR(1), WILD14 CHAR(1), WILD18 CHAR(1), WCUSTN CHAR(3), -                           
        WILDI1 CHAR(120), WILCI1 CHAR(60), WILZI1 CHAR(60))';                                       
                                                                                                    
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur From :SqlNr1;                                                               
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur;                                                                           
                                                                                                    
          SqlNr1 = *Blanks;                                                                         
          SqlNr1 = 'Delete From Votrea'+ ciao + '/vtwilbfA';                                        
           Exec Sql                                                                                 
            Prepare Cur1 From :SqlNr1;                                                              
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur1;                                                                          
                                                                                                    
                                                                                                    
         SqlNr1 = *Blanks;                                                                          
         // Crea Tabla  vtwilbfD   para guardar Los DAF                                             
         // DLM, COR, RCD, RIP, MLI                                                                 
         SqlNr1 = 'Create Table Votrea'+ ciao + '/vtwilbfD (' +                                     
        'WILZON CHAR(10), -                                                                         
        WILCED NUMERIC(9), WILRUC NUMERIC(15), WILRUA CHAR(30), -                                   
        WILNOM CHAR(30), WILCUS CHAR(3), -                                                          
        WILDIR CHAR(180), WILCTY CHAR(20), WILTL1 NUMERIC(15), -                                    
        WILTL2 NUMERIC(15), WILFEC CHAR(10), WILVAL NUMERIC(14, 2), -                               
        WILKAP NUMERIC(15, 2), WILMDO CHAR(10), WILMDL CHAR(10), -                                  
        WILCRD NUMERIC(9), WILINF CHAR(10), WILDEL CHAR(1), -                                       
        WILMLD CHAR(1), WILDI2 CHAR(180), WILBRD CHAR(30), -                                        
        WILD02 CHAR(1), WILD03 CHAR(1), WILD06 CHAR(1), -                                           
        WILD09 CHAR(1), WILD14 CHAR(1), WILD18 CHAR(1), WCUSTN CHAR(3), -                           
        WILDI1 CHAR(120), WILCI1 CHAR(60), WILZI1 CHAR(60))';                                       
                                                                                                    
                                                                                                    
           Exec Sql                                                                                 
            Prepare Cur From :SqlNr1;                                                               
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur;                                                                           
                                                                                                    
          SqlNr1 = *Blanks;                                                                         
          SqlNr1 = 'Delete From Votrea'+ ciao + '/vtwilbfD';                                        
           Exec Sql                                                                                 
            Prepare Cur1 From :SqlNr1;                                                              
                                                                                                    
           Exec Sql                                                                                 
             Execute Cur1;                                                                          
                                                                                                    
      /end-free                                                                                     
     C*Inserta los registros donde MLZIPC <> 'WEB', 'AMA', 'LIN', 'DAF'                             
     C*DLM, COR, RCD, RIP, MLI                                                                      
     C/Exec Sql                                                                                     
     C+ INSERT INTO VTWILBF (WILZON, WILCED, WILRUC, WILRUA,                                        
     C+ WILNOM, WILCUS,                                                                             
     C+ WILDIR, WILCTY, WILTL1, WILTL2, WILFEC, WILVAL, WILMDO, WILMDL,                             
     C+ WILCRD, WILINF, WILDEL, WILMLD, WILDI2,                                                     
     C+ WILBRD, WILD02, WILD03, WILD06, WILD09, WILD14, WILD18, WCUSTN,                             
     C+ WILDI1, WILCI1, WILZI1, WILKAP)                                                             
     C+ SELECT MLZIPC, WI1CED, MLTFAX, MLPOST, MLNAME, MLPRLV, OTODIR,                              
     C+ MLCITY, MLTEL1, MLTEL2, WI1FEC,                                                             
     c+ WI1SDO, MLOMED, MLLMED, MLCRLM, SMINF3, MLDELE, MLDM01, OTODID,                             
     C+ OTOBRD, MLDM02, MLDM03, MLDM06, MLDM09, MLDM14, MLDM18, MLCLAS,                             
     C+ IFNULL(OT1DIR, ' '), IFNULL(OT1CIU, ' '), IFNULL(OT1ZIP, ' '),                              
     C+ WI1VAL                                                                                      
     C+ FROM VTWI1BF                                                                                
     C+ INNER JOIN LMLLST01 ON WI1CED = MLCSTÑ INNER JOIN PSLMAN                                    
     C+ ON SMSLSP = MLZIPC  INNER JOIN VTMOTOF ON OTOCED=MLCSTÑ                                     
     C+ LEFT JOIN VTMOT1F  ON WI1CED = OT1COD                                                       
     C+ WHERE                                                                                       
     C+ MLZIPC NOT IN('WEB', 'AMA', 'LIN', 'DAF',                                                   
     C+ 'DLM', 'COR', 'RCD', 'RIP', 'MLI' )                                                         
     C/End-Exec                                                                                     
                                                                                                    
     C*Inserta los registros donde MLZIPC =  'WEB'                                                  
     C/Exec Sql                                                                                     
     C+ INSERT INTO VTWILBFW (WILZON, WILCED, WILRUC, WILRUA,                                       
     C+ WILNOM, WILCUS,                                                                             
     C+ WILDIR, WILCTY, WILTL1, WILTL2, WILFEC, WILVAL, WILMDO, WILMDL,                             
     C+ WILCRD, WILINF, WILDEL, WILMLD, WILDI2,                                                     
     C+ WILBRD, WILD02, WILD03, WILD06, WILD09, WILD14, WILD18, WCUSTN,                             
     C+ WILDI1, WILCI1, WILZI1, WILKAP)                                                             
     C+ SELECT MLZIPC, WI1CED, MLTFAX, MLPOST, MLNAME, MLPRLV, OTODIR,                              
     C+ MLCITY, MLTEL1, MLTEL2, WI1FEC,                                                             
     c+ WI1SDO, MLOMED, MLLMED, MLCRLM, SMINF3, MLDELE, MLDM01, OTODID,                             
     C+ OTOBRD, MLDM02, MLDM03, MLDM06, MLDM09, MLDM14, MLDM18, MLCLAS,                             
     C+ IFNULL(OT1DIR, ' '), IFNULL(OT1CIU, ' '), IFNULL(OT1ZIP, ' '),                              
     C+ 0                                                                                           
     C+ FROM VTWI1BF                                                                                
     C+ INNER JOIN LMLLST01 ON WI1CED = MLCSTÑ INNER JOIN PSLMAN                                    
     C+ ON SMSLSP = MLZIPC  INNER JOIN VTMOTOF ON OTOCED=MLCSTÑ                                     
     C+ LEFT JOIN VTMOT1F  ON WI1CED = OT1COD                                                       
     C+ WHERE MLZIPC =  'WEB'                                                                       
     C/End-Exec                                                                                     
                                                                                                    
     C*Inserta los registros donde MLZIPC =  'AMA'                                                  
     C/Exec Sql                                                                                     
     C+ INSERT INTO VTWILBFA (WILZON, WILCED, WILRUC, WILRUA,                                       
     C+ WILNOM, WILCUS,                                                                             
     C+ WILDIR, WILCTY, WILTL1, WILTL2, WILFEC, WILVAL, WILMDO, WILMDL,                             
     C+ WILCRD, WILINF, WILDEL, WILMLD, WILDI2,                                                     
     C+ WILBRD, WILD02, WILD03, WILD06, WILD09, WILD14, WILD18, WCUSTN,                             
     C+ WILDI1, WILCI1, WILZI1, WILKAP)                                                             
     C+ SELECT MLZIPC, WI1CED, MLTFAX, MLPOST, MLNAME, MLPRLV, OTODIR,                              
     C+ MLCITY, MLTEL1, MLTEL2, WI1FEC,                                                             
     c+ WI1SDO, MLOMED, MLLMED, MLCRLM, SMINF3, MLDELE, MLDM01, OTODID,                             
     C+ OTOBRD, MLDM02, MLDM03, MLDM06, MLDM09, MLDM14, MLDM18, MLCLAS,                             
     C+ IFNULL(OT1DIR, ' '), IFNULL(OT1CIU, ' '), IFNULL(OT1ZIP, ' '),                              
     C+ 0                                                                                           
     C+ FROM VTWI1BF                                                                                
     C+ INNER JOIN LMLLST01 ON WI1CED = MLCSTÑ INNER JOIN PSLMAN                                    
     C+ ON SMSLSP = MLZIPC  INNER JOIN VTMOTOF ON OTOCED=MLCSTÑ                                     
     C+ LEFT JOIN VTMOT1F  ON WI1CED = OT1COD                                                       
     C+ WHERE MLZIPC =  'AMA'                                                                       
     C/End-Exec                                                                                     
                                                                                                    
                                                                                                    
     C*Inserta los registros donde MLZIPC IN 'DAF'                                                  
     C* DLM, COR, RCD, RIP, MLI                                                                     
     C/Exec Sql                                                                                     
     C+ INSERT INTO VTWILBFD (WILZON, WILCED, WILRUC, WILRUA,                                       
     C+ WILNOM, WILCUS,                                                                             
     C+ WILDIR, WILCTY, WILTL1, WILTL2, WILFEC, WILVAL, WILMDO, WILMDL,                             
     C+ WILCRD, WILINF, WILDEL, WILMLD, WILDI2,                                                     
     C+ WILBRD, WILD02, WILD03, WILD06, WILD09, WILD14, WILD18, WCUSTN,                             
     C+ WILDI1, WILCI1, WILZI1, WILKAP)                                                             
     C+ SELECT MLZIPC, WI1CED, MLTFAX, MLPOST, MLNAME, MLPRLV, OTODIR,                              
     C+ MLCITY, MLTEL1, MLTEL2, WI1FEC,                                                             
     c+ WI1SDO, MLOMED, MLLMED, MLCRLM, SMINF3, MLDELE, MLDM01, OTODID,                             
     C+ OTOBRD, MLDM02, MLDM03, MLDM06, MLDM09, MLDM14, MLDM18, MLCLAS,                             
     C+ IFNULL(OT1DIR, ' '), IFNULL(OT1CIU, ' '), IFNULL(OT1ZIP, ' '),                              
     C+ 0                                                                                           
     C+ FROM VTWI1BF                                                                                
     C+ INNER JOIN LMLLST01 ON WI1CED = MLCSTÑ INNER JOIN PSLMAN                                    
     C+ ON SMSLSP = MLZIPC  INNER JOIN VTMOTOF ON OTOCED=MLCSTÑ                                     
     C+ LEFT JOIN VTMOT1F  ON WI1CED = OT1COD                                                       
     C+ WHERE MLZIPC IN ('DAF', 'DLM', 'COR', 'RCD', 'RIP', 'MLI')                                  
     C/End-Exec                                                                                     
                                                                                                    
      /free                                                                                         
       Pmensaje = 'Fin Suba Excel Arc(Vtwilbf,VtwilbfW,VtwilbfA, VtwilbfD)';                        
       Pmensaje1 = 'que esta en la Libreria (Votrea' + Ciao + '), Pulse Intro.';                    
       Exfmt Rdato2;                                                                                
       *inlr=*on;                                                                                   
      /end-free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Pinta el Sub Archivo de la Pantalla            //                       
       //------------------------------------------------------------------//                       
     PPrSaldos         B                                                                            
     DPrSaldos         Pi                                                                           
     C                   Z-ADD     0             BALCAR                                             
     C                   MOVE      *BLANKS       FECCOM                                             
     C                   MOVE      *ZEROS        SW                1 0                              
     C                   MOVE      *ZEROS        W_TotKap                                           
     C     AdcstÑ        SETLL     LARDTL01                                                         
     C     AdcstÑ        READE     LARDTL01                               40                        
                                                                                                    
     C     *IN40         DOWEQ     *OFF                                                             
     C                   ADD       ADTRAM        BALCAR                                             
      /Free                                                                                         
       If W_indProc = 'S';                                                                          
         If ADRDPT = TCoiCoh Or ADRDPT = TCoiCoi Or ADRDPT = TCoiCod Or                             
            ADRDPT = TCoiCpi;                                                                       
           If ADRDPT  <> 0;                                                                         
              W_TotKap = W_TotKap + AdtRam;                                                         
           EndIf;                                                                                   
         Else;                                                                                      
           W_TotKap = W_TotKap;                                                                     
         EndIf;                                                                                     
       EndIf;                                                                                       
      /End-Free                                                                                     
                                                                                                    
     C     ADTRTY        IFEQ      'I'                                                              
     C     SW            ANDEQ     0                                                                
     C                   EVAL      OSIGLO = %CHAR(ADDUCC)                                           
     C                   EVALR     OAÑO = %CHAR(ADDUYY)                                             
     C                   EVALR     OMES = %CHAR(ADDUMM)                                             
     C                   EVALR     ODIA = %CHAR(ADDUDD)                                             
     C                   EVAL      OSIGLO = %XLATE(' ':'0':OSIGLO)                                  
     C                   EVAL      OAÑO = %XLATE(' ':'0':OAÑO)                                      
     C                   EVAL      OMES = %XLATE(' ':'0':OMES)                                      
     C                   EVAL      ODIA = %XLATE(' ':'0':ODIA)                                      
     C                   MOVE      1             SW                                                 
     C                   ENDIF                                                                      
     C     AdcstÑ        READE     LARDTL01                               40                        
     C                   ENDDO                                                                      
     PPrSaldos         E                                                                            
