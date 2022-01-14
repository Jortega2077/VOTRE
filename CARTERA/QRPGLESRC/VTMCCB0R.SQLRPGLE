      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCB0R                                          **                        
      * Descripción   : Programa que Genera las Compradoras Activas x Zna **                        
      * Proyecto      : Menu Opciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlNr1           s          20000a   Inz(*Blanks)                                             
     DSqlNr2           s          20000a   Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DWPos             s              5s 0 Inz(*Zeros)                                              
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DEstructura       Ds                                                                           
     DMLZIPC                         10a                                                            
     DMLCSTÑ                          9  0                                                          
     DMLTFAX                         30                                                             
     DMLPOST                         30                                                             
     DMLNAME                         30                                                             
     DMLCLAS                          3                                                             
     DMLTEL1                         15  0                                                          
     DMLTEL2                         15  0                                                          
     DOTOCEL                         11  0                                                          
     DMLOMED                         10                                                             
     DMLLMED                         10                                                             
     DMLCRLM                          9  0                                                          
     DMLDM01                          1                                                             
     DMLDM03                          1                                                             
     DMLDELE                          1                                                             
     DMEEMAD                         60                                                             
     DOTODIR                        180                                                             
     DOTODTR                          2                                                             
     DOTOCDR                         20                                                             
     DOTOBRR                         30                                                             
     DOTOCPR                         10                                                             
     DPRRPARV                        60                                                             
     DOTOCDD                         20                                                             
     DOTODID                        180                                                             
     DOTODTD                          2                                                             
     DOTOBRD                         30                                                             
     DOTOCPD                         10                                                             
     DPRRPARD                        60                                                             
     DMLPRLV                          3                                                             
     DOT1DIR                        120                                                             
     DOT1CIU                         60                                                             
     DOT1ZIP                         60                                                             
     DMLDM01c                         1                                                             
     DMLDM02                          1                                                             
     DMLDM03c                         1                                                             
     DMLDM04                          1                                                             
     DMLDM05                          1                                                             
     DMLDM06                          1                                                             
     DMLDM07                          1                                                             
     DMLDM08                          1                                                             
     DMLDM09                          1                                                             
     DMLDM10                          1                                                             
     DMLDM11                          1                                                             
     DMLDM12                          1                                                             
     DMLDM13                          1                                                             
     DMLDM14                          1                                                             
     DMLDM15                          1                                                             
     DMLDM16                          1                                                             
     DMLDM17                          1                                                             
     DMLDM18                          1                                                             
     DMLDM19                          1                                                             
     DMLDM20                          1                                                             
     DMSDESC                         40                                                             
     DSMINF3                          1                                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DInsertarFile     Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
     DBorrarFile       Pr                                                                           
     DPrFile           Pr              n                                                            
                                                                                                    
     DMain             Pr                  Extpgm('VTMCCB0R')                                       
     DPcia                            3a                                                            
     DUser                           10a                                                            
     DPZon                         8000a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DUser                           10a                                                            
     DPZon                         8000a                                                            
                                                                                                    
     d SleepMinutes    s             10i 0 inz(2)                                                   
     d sleep           Pr            10i 0 ExtProc( 'sleep' )                                       
     d  seconds                      10u 0 Value                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
        Wpos = %Scan('|':Pzon);                                                                     
        If Wpos > *Zeros;                                                                           
         SqlNr2 = *Blanks;                                                                          
         SqlNr2 = %Subst(Pzon:1:Wpos-1);                                                            
        Endif;                                                                                      
        Sleep(SleepMinutes);                                                                        
        PgmPais(Pcia:Xpais);                                                                        
        InsertarFile();                                                                             
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Inserta Datos al Archivo de Trabajo            //                       
       //------------------------------------------------------------------//                       
     PInsertarFile     B                                                                            
     DInsertarFile     Pi                                                                           
      /Free                                                                                         
       BorrarFile();                                                                                
       SqlNr1 = *Blanks;                                                                            
       If %Len(%Trim(SqlNr2)) > *Zeros;                                                             
       SqlNr1 = 'SELECT DISTINCT ' +                                                                
         'MLZIPC, IFNULL(MLCSTÑ, 0), MLTFAX, MLPOST, MLNAME, MLCLAS, ' +                            
         'MLTEL1, MLTEL2, ' +                                                                       
       'IFNULL(OTOCEL, 0), ' +                                                                      
       'MLOMED, MLLMED, MLCRLM, MLDM01, MLDM03, MLDELE, ' +                                         
       'IFNULL(MEEMAD, '' ''' +' ), ' +                                                             
       'IFNULL(OTODIR, '' ''' +' ), '+                                                              
       'IFNULL(OTODTR, '' ''' +' ), '+                                                              
       'IFNULL(OTOCDR, '' ''' +' ), '+                                                              
       'IFNULL(OTOBRR, '' ''' +' ), '+                                                              
       'IFNULL(OTOCPR, '' ''' +' ), '+                                                              
       'IFNULL(A.PRRPAR, '' ''' +'), ' +                                                            
       'IFNULL(OTOCDD, '' ''' +' ), '+                                                              
       'IFNULL(OTODID, '' ''' +' ), '+                                                              
       'IFNULL(OTODTD, '' ''' +' ), '+                                                              
       'IFNULL(OTOBRD, '' ''' +' ), '+                                                              
       'IFNULL(OTOCPD, '' ''' +' ), '+                                                              
       'IFNULL(B.PRRPAR, '' ''' +'), ' +                                                            
                'MLPRLV, ' +                                                                        
       'IFNULL(OT1DIR, '' ''' +'), ' +                                                              
       'IFNULL(OT1CIU, '' ''' +'), ' +                                                              
       'IFNULL(OT1ZIP, '' ''' +'), ' +                                                              
                'MLDM01, MLDM02, MLDM03, MLDM04, MLDM05, MLDM06, ' +                                
                'MLDM07, MLDM08, MLDM09, MLDM10, MLDM11, MLDM12, ' +                                
                'MLDM13, MLDM14, MLDM15, MLDM16, MLDM17, MLDM18, ' +                                
                'MLDM19, MLDM20, MSDESC, TRIM(SMINF3) ' +                                           
                'FROM OPF' + PCIA + '/LMLLST01 ' +                                                  
                'LEFT JOIN OPF' + PCIA + '/LMLEML01 ON MECSTÑ=MLCSTÑ ' +                            
                'LEFT JOIN VOTREA' + PCIA + '/PARDTLW ON ADCSTÑ=MLCSTÑ ' +                          
                'LEFT JOIN OPCUS' + PCIA + '/VTMOTOF  ON OTOCED=MLCSTÑ ' +                          
                'LEFT JOIN VOTREA' + PCIA + '/VTMOT1F ON OT1COD=MLCSTÑ ' +                          
          'LEFT JOIN VOTREA' + PCIA + '/VTMPRRF A ON OT1PRV=A.PRRCPA ' +                            
          'LEFT JOIN VOTREA' + PCIA + '/VTMPRRF B ON OT1PRD=B.PRRCPA ' +                            
                'INNER JOIN OPF' + PCIA + '/LMLSTA01 ON MLSTAT = MSSTAC ' +                         
                'INNER JOIN OPF' + PCIA + '/LSLMAN01 ON SMSLSP = MLZIPC ' +                         
                'WHERE ' + %Trim(SqlNr2) + ' Order By Mlzipc';                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :SqlNr1;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor0 Cursor For Cur0;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor0;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :Estructura;                                                             
                                                                                                    
       Dow Sqlcod <> 100;                                                                           
        If PrFile() = *On;                                                                          
         SqlNr1 = 'Insert Into ' + %Trim(User) + '/VTWI2BF ' +                                      
                  '(WI2ZON, WI2CED, WI2CLL, WI2POS, WI2NAM, WI2CLS, WI2TL1, ' +                     
                  'WI2TL2, WI2CEL, WI2MED, WI2ME1, WI2CRL, WI2DM1, ' +                              
            'WI2DM3, WI2DEL, WI2EMA, WI2DIR, WI2DTR, WI2CDR, WI2BRR, WI2CPR, ' +                    
                  'WI2PRR, ' +                                                                      
                  'WI2CDD, WI2DID, WI2DTD, WI2BRD, WI2CPD, ' +                                      
                  'WI2PRD, ' +                                                                      
                  'WI2PRL, WI2DPB, WI2CPB, WI2ZPB, ' +                                              
                  'WI2D01, WI2DM2, WI2D03, WI2DM4, WI2DM5, WI2DM6, ' +                              
                  'WI2DM7, WI2DM8, WI2DM9, WI2D10, WI2D11, WI2D12, ' +                              
                  'WI2D13, WI2D14, WI2D15, WI2D16, WI2D17, WI2D18, ' +                              
                  'WI2D19, WI2D20, WI2DTP, WI2MLP)' +                                               
                  ' SELECT DISTINCT ' +                                                             
         'MLZIPC, IFNULL(MLCSTÑ, 0), MLTFAX, MLPOST, MLNAME, MLCLAS, ' +                            
         'MLTEL1, MLTEL2, ' +                                                                       
       'IFNULL(OTOCEL, 0), ' +                                                                      
        'MLOMED, MLLMED, MLCRLM, MLDM01, MLDM03, MLDELE, ' +                                        
        'IFNULL(MEEMAD, '' ''' + '), '+                                                             
       'IFNULL(OTODIR, '' ''' + '), ' +                                                             
       'IFNULL(OTODTR, '' ''' + '), ' +                                                             
       'IFNULL(OTOCDR, '' ''' + '), ' +                                                             
       'IFNULL(OTOBRR, '' ''' + '), ' +                                                             
       'IFNULL(OTOCPR, '' ''' + '), ' +                                                             
       'IFNULL(A.PRRPAR, '' ''' +'), ' +                                                            
       'IFNULL(OTOCDD, '' ''' + '), ' +                                                             
       'IFNULL(OTODID, '' ''' + '), ' +                                                             
       'IFNULL(OTODTD, '' ''' + '), ' +                                                             
       'IFNULL(OTOBRD, '' ''' + '), ' +                                                             
       'IFNULL(OTOCPD, '' ''' + '), ' +                                                             
       'IFNULL(B.PRRPAR, '' ''' +'), ' +                                                            
                'MLPRLV, ' +                                                                        
       'IFNULL(OT1DIR, '' ''' +'), ' +                                                              
       'IFNULL(OT1CIU, '' ''' +'), ' +                                                              
       'IFNULL(OT1ZIP, '' ''' +'), ' +                                                              
                'MLDM01, MLDM02, MLDM03, MLDM04, MLDM05, MLDM06, ' +                                
                'MLDM07, MLDM08, MLDM09, MLDM10, MLDM11, MLDM12, ' +                                
                'MLDM13, MLDM14, MLDM15, MLDM16, MLDM17, MLDM18, ' +                                
                'MLDM19, MLDM20, MSDESC, TRIM(SMINF3) ' +                                           
                'FROM OPF' + PCIA + '/LMLLST01 ' +                                                  
                'LEFT JOIN OPF' + PCIA + '/LMLEML01 ON MECSTÑ=MLCSTÑ ' +                            
                'LEFT JOIN VOTREA' + PCIA + '/PARDTLW ON ADCSTÑ=MLCSTÑ ' +                          
                'LEFT JOIN OPCUS' + PCIA + '/VTMOTOF  ON OTOCED=MLCSTÑ ' +                          
                'LEFT JOIN VOTREA' + PCIA + '/VTMOT1F ON OT1COD=MLCSTÑ ' +                          
          'LEFT  JOIN VOTREA' + PCIA + '/VTMPRRF A ON OT1PRV=A.PRRCPA ' +                           
          'LEFT  JOIN VOTREA' + PCIA + '/VTMPRRF B ON OT1PRD=B.PRRCPA ' +                           
                'INNER JOIN OPF' + PCIA + '/LMLSTA01 ON MLSTAT = MSSTAC ' +                         
                'INNER JOIN OPF' + PCIA + '/LSLMAN01 ON SMSLSP = MLZIPC ' +                         
                'WHERE ' + %Trim(SqlNr2) + ' And MlcstÑ = ' +                                       
                %Trim(%Editc(MlcstÑ:'Z'));                                                          
                                                                                                    
       Monitor;                                                                                     
        Exec Sql                                                                                    
         Execute Immediate :SqlNr1;                                                                 
       On-Error;                                                                                    
       Endmon;                                                                                      
                                                                                                    
        Endif;                                                                                      
        Exec Sql                                                                                    
         Fetch Cursor0 Into :Estructura;                                                            
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor0;                                                                              
                                                                                                    
       Endif;                                                                                       
      /End-Free                                                                                     
     PInsertarFile     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Archivo de Trabajo por Compañia          //                       
       //------------------------------------------------------------------//                       
     PBorrarFile       B                                                                            
     DBorrarFile       Pi                                                                           
      /Free                                                                                         
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Delete From ' +  %Trim(User) + '/VTWI2BF ';                                        
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur3 From :SqlNr1;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Execute Cur3;                                                                              
      /End-Free                                                                                     
     PBorrarFile       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Consulta si la Cedula ya Existe en el Archivo  //                       
       //------------------------------------------------------------------//                       
     PPrFile           B                                                                            
     DPrFile           Pi              n                                                            
     ** Definiciòn de Variables de Trabajo                                                          
     DPsw              s               n   Inz(*On)                                                 
     DNro              s              2  0 Inz(*Zeros)                                              
      /Free                                                                                         
       If MlcstÑ > *Zeros;                                                                          
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Select Count(0) ' +                                                                
                'From ' + %Trim(User) + '/Vtwi2bf ' +                                               
                'Where Wi2ced = ' + %Trim(%Editc(MlcstÑ:'Z'));                                      
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur1 From :SqlNr1;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor1 Cursor For Cur1;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor1;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor1 Into :Nro;                                                                    
                                                                                                    
       If Sqlcod <> 100 And Sqlcod >= *Zeros;                                                       
        If Nro > *Zeros;                                                                            
         Psw = *Off;                                                                                
        Endif;                                                                                      
       Endif;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close Cursor1;                                                                              
                                                                                                    
       Endif;                                                                                       
                                                                                                    
       Return Psw;                                                                                  
      /End-Free                                                                                     
     PPrFile           E                                                                            
