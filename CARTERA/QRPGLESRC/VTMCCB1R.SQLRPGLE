      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCB1R                                          **                        
      * Descripción   : Programa que Genera las Compradoras Inactiv x Zna **                        
      * Proyecto      : Menu Opciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
     ** Fecha Creaciòn: 12 de Abril de 2013                               **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlNr1           s           5000a   Inz(*Blanks)                                             
     DSqlNr2           s           5000a   Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DWPos             s              5s 0 Inz(*Zeros)                                              
     DValor            s           2000a   Inz(*Blanks)                                             
                                                                                                    
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
     DOTOCDD                         20                                                             
     DOTODID                        180                                                             
     DOTODTD                          2                                                             
     DOTOBRD                         30                                                             
     DOTOCPD                         10                                                             
     DMLPRLV                          3                                                             
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
     DPrBorrarDobles   Pr                                                                           
                                                                                                    
     DMain             Pr                  Extpgm('VTMCCB0R')                                       
     DPcia                            3a                                                            
     DUser                           10a                                                            
     DPZon                         1000a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DUser                           10a                                                            
     DPZon                         1000a                                                            
                                                                                                    
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
        PrBorrarDobles();                                                                           
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
       'MLZIPC, MLCSTÑ, MLTFAX, MLPOST, MLNAME, MLCLAS, MLTEL1, MLTEL2, ' +                         
       'IFNULL(OTOCEL, 0), ' +                                                                      
       'MLOMED, MLLMED, MLCRLM, MLDM01, MLDM03, MLDELE, ' +                                         
       'IFNULL(MEEMAD, '' ''' +                                                                     
       '), IFNULL(OTODIR, '' ''' + '), IFNULL(OTODTR, '' ''' + '), ' +                              
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
         'MLDM01, MLDM02, MLDM03, MLDM04, MLDM05, MLDM06, ' +                                       
         'MLDM07, MLDM08, MLDM09, MLDM10, MLDM11, MLDM12, ' +                                       
         'MLDM13, MLDM14, MLDM15, MLDM16, MLDM17, MLDM18, ' +                                       
         'MLDM19, MLDM20, MSDESC, TRIM(SMINF3) ' +                                                  
         'FROM OPF' + PCIA + '/LMLLST01 ' +                                                         
         'LEFT JOIN OPF' + PCIA + '/LMLEML01 ON MECSTÑ=MLCSTÑ  ' +                                  
         'LEFT JOIN VOTREA' + PCIA + '/PARDTLW ON ADCSTÑ=MLCSTÑ ' +                                 
         'LEFT JOIN OPCUS' + PCIA + '/VTMOTOF  ON OTOCED=MLCSTÑ ' +                                 
         'LEFT JOIN VOTREA' + PCIA + '/VTMOT1F ON OT1COD=MLCSTÑ ' +                                 
         'LEFT JOIN VOTREA' + PCIA + '/VTMPRRF A ON OT1PRV=A.PRRCPA ' +                             
         'LEFT JOIN VOTREA' + PCIA + '/VTMPRRF B ON OT1PRD=B.PRRCPA ' +                             
         'INNER JOIN OPF' + PCIA + '/LMLSTA01 ON MLSTAT = MSSTAC ' +                                
         'INNER JOIN OPF' + PCIA + '/LSLMAN01 ON SMSLSP = MLZIPC ' +                                
         'WHERE ' + %Trim(SqlNr2);                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Prepare Cur0 From :SqlNr1;                                                                  
                                                                                                    
       Exec Sql                                                                                     
        Declare Cursor0 Cursor For Cur0;                                                            
                                                                                                    
       Exec Sql                                                                                     
        Open Cursor0;                                                                               
                                                                                                    
       Exec Sql                                                                                     
        Fetch Cursor0 Into :Estructura;                                                             
                                                                                                    
       Dow Sqlcod <> 100 And Sqlcod >= *Zeros;                                                      
        If PrFile = *On;                                                                            
        SqlNr1 = 'Insert Into ' + %Trim(User) + '/VTWI4BF ' +                                       
          '(WI4ZON, WI4CED, WI4CLL, WI4POS, WI4NAM, WI4CLS, WI4TL1, ' +                             
          'WI4TL2, WI4CEL, WI4MED, WI4ME1, WI4CRL, WI4DM1, ' +                                      
          'WI4DM3, WI4DEL, WI4EMA, WI4DIR, WI4DTR, WI4CDR, WI4BRR, WI4CPR, ' +                      
          'WI4PRR, ' +                                                                              
          'WI4CDD, WI4DID, WI4DTD, WI4BRD, WI4CPD, ' +                                              
          'WI4PRD, ' +                                                                              
          'WI4PRL, ' +                                                                              
          'WI4D01, WI4DM2, WI4D03, WI4DM4, WI4DM5, WI4DM6, ' +                                      
          'WI4DM7, WI4DM8, WI4DM9, WI4D10, WI4D11, WI4D12, ' +                                      
          'WI4D13, WI4D14, WI4D15, WI4D16, WI4D17, WI4D18, ' +                                      
          'WI4D19, WI4D20, WI4DTP, WI4MLP)' +                                                       
          'SELECT DISTINCT ' +                                                                      
          'MLZIPC, MLCSTÑ, MLTFAX, MLPOST, MLNAME, MLCLAS, MLTEL1, MLTEL2, ' +                      
          'IFNULL(OTOCEL, 0), ' +                                                                   
          'MLOMED, MLLMED, MLCRLM, MLDM01, MLDM03, MLDELE, ' +                                      
          'IFNULL(MEEMAD, '' ''' +                                                                  
          '), IFNULL(OTODIR, '' ''' + '), IFNULL(OTODTR, '' ''' + '), ' +                           
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
          'MLDM01, MLDM02, MLDM03, MLDM04, MLDM05, MLDM06, ' +                                      
          'MLDM07, MLDM08, MLDM09, MLDM10, MLDM11, MLDM12, ' +                                      
          'MLDM13, MLDM14, MLDM15, MLDM16, MLDM17, MLDM18, ' +                                      
          'MLDM19, MLDM20, MSDESC, TRIM(SMINF3) ' +                                                 
          'FROM OPF' + PCIA + '/LMLLST01 ' +                                                        
          'LEFT JOIN OPF' + PCIA + '/LMLEML01 ON MECSTÑ=MLCSTÑ  ' +                                 
          'LEFT JOIN VOTREA' + PCIA + '/PARDTLW ON ADCSTÑ=MLCSTÑ ' +                                
          'LEFT JOIN OPCUS' + PCIA + '/VTMOTOF  ON OTOCED=MLCSTÑ ' +                                
          'LEFT JOIN VOTREA' + PCIA + '/VTMOT1F ON OT1COD=MLCSTÑ ' +                                
          'LEFT  JOIN VOTREA' + PCIA + '/VTMPRRF A ON OT1PRV=A.PRRCPA ' +                           
          'LEFT  JOIN VOTREA' + PCIA + '/VTMPRRF B ON OT1PRD=B.PRRCPA ' +                           
          'INNER JOIN OPF' + PCIA + '/LMLSTA01 ON MLSTAT = MSSTAC ' +                               
          'INNER JOIN OPF' + PCIA + '/LSLMAN01 ON SMSLSP = MLZIPC ' +                               
          'WHERE ' + %Trim(SqlNr2) + ' And MlcstÑ = ' +                                             
          %Trim(%Editc(MlcstÑ:'Z'));                                                                
                                                                                                    
        Exec Sql                                                                                    
         Execute Immediate :SqlNr1;                                                                 
                                                                                                    
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
       SqlNr1 = 'Delete From ' +  %Trim(User) + '/VTWI4BF ';                                        
                                                                                                    
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
     DNro              s              9  0 Inz(*Zeros)                                              
      /Free                                                                                         
                                                                                                    
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Select Count(0) ' +                                                                
                'From ' + %Trim(User) + '/Vtwi4bf ' +                                               
                'Where Wi4ced = ' + %Trim(%Editc(MlcstÑ:'Z'));                                      
                                                                                                    
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
                                                                                                    
       Return Psw;                                                                                  
      /End-Free                                                                                     
     PPrFile           E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Registros Dobles de una Tabla            //                       
       //------------------------------------------------------------------//                       
     PPrBorrarDobles   B                                                                            
     DPrBorrarDobles   Pi                                                                           
      /Free                                                                                         
                                                                                                    
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Delete From ' + %Trim(User) + '/Vtwi4bf A ' +                                      
                'Where Rrn(A) > ' +                                                                 
                '(Select Min(Rrn(B)) From ' + %Trim(User) + '/Vtwi4bf B ' +                         
                'Where A.Wi4Ced = B.Wi4Ced)';                                                       
                                                                                                    
       Exec Sql                                                                                     
        Execute Immediate :SqlNr1;                                                                  
                                                                                                    
      /End-Free                                                                                     
     PPrBorrarDobles   E                                                                            
