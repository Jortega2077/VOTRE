      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCER                                          **                        
      * Descripción   : Programa que Genera las Inscripciones Diarias     **                        
      * Proyecto      : Menu Opciones OPI Paises                          **                        
      * Autor         : (Jortega)                                         **                        
      **********************************************************************                        
      * Modificación  : Se proceso pantalla del proceso de selección de   **                        
      *                 la información.                                   **                        
      * Autor         : (Personalsoft - Maria Feria)                      **                        
      * Fecha Creación: 06 de Abril de 2010                               **                        
      **********************************************************************                        
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     DSqlNr1           s           8000a   Inz(*Blanks)                                             
     DSqlNr2           s           8000a   Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
     DPtr              s               *   Inz(%Addr(*In))                                          
     DUser             s             10a   Inz(*User)                                               
     DStrsql           s           5000a   Inz(*Blanks)                                             
     DPcampa           s              4a   Inz(*Blanks)                                             
     DWPos             s              5s 0 Inz(*Zeros)                                              
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DBorrarFile       Pr                                                                           
     DInsertarFile     Pr                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DPCam                            4a                                                            
     DPZon                         8000a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DPcam                            4a                                                            
     DPZon                         8000a                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
        PgmPais(Pcia:Xpais);                                                                        
        WPos = %Scan(')':Pzon:1);                                                                   
        SqlNr2 = %SubSt(Pzon:1:WPos);                                                               
        Pcampa = Pcam;                                                                              
        InsertarFile();                                                                             
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Inserta Datos al Archivo de Trabajo            //                       
       //------------------------------------------------------------------//                       
     PInsertarFile     B                                                                            
     DInsertarFile     Pi                                                                           
      ** Definicion de Variables de Trabajo                                                         
     DWblanc           s              1a   Inz(*Blanks)                                             
     DWohcmf           s              1a   Inz('N')                                                 
     DWohrcs           s              1a   Inz('C')                                                 
     DWdemo1           s              1a   Inz('N')                                                 
     DWdemo2           s              1a   Inz('R')                                                 
     DWdemo3           s              1a   Inz('T')                                                 
     DWBlan            s              1a   Inz(' ')                                                 
      /Free                                                                                         
                                                                                                    
       BorrarFile();                                                                                
       SqlNr1 = *Blanks;                                                                            
       If %Len(%Trim(SqlNr2)) > *Zeros;                                                             
       SqlNr1 = 'Insert Into ' + %Trim(User) + '/Vtwisdf (Isddel, Isdcia, -                         
                 Isdzon, Isdced, Isdcdl, -                                                          
                 Isdpos, Isdnom, Isdcus, Isddmg, Isdddr, Isddrd, Isdciu, -                          
                 Isdcrt, Isdcor, Isdcbj, Isdest, ' +                                                
                'Isddm1, Isddm2, Isddm3, Isddm4, ' +                                                
                'Isddm5, Isddm6, Isddm7, Isddm8, ' +                                                
                'Isddm9, Isdd10, Isdd11, Isdd12, ' +                                                
                'Isdd13, Isdd14, Isdd15, Isdd16, ' +                                                
                'Isdd17, Isdd18, Isdd19, Isdd20, Isdmpl) ' +                                        
                'Select A.Mldele, ' + '''' + Pcia + '''' + ', A.Mlzipc, -                           
                A.MlcstÑ, -                                                                         
                A.Mltfax, -                                                                         
                A.Mlpost, A.Mlname, A.Mlprlv, -                                                     
                A.Mldm19, C.Otodir, C.Otodid, A.Mlcity, A.Mlcrlm, -                                 
                 A.Mlomed, Substr(Mlinf1, 7, 4) As BajaCampa' +                                     
               ', Ifnull(D.Reiesc,' + '''' + WBlan + '''' +') As CO, ' +                            
               'A.Mldm01, A.Mldm02, A.Mldm03, A.Mldm04, A.Mldm05, ' +                               
               'A.Mldm06, A.Mldm07, A.Mldm08, A.Mldm09, A.Mldm10, ' +                               
               'A.Mldm11, A.Mldm12, A.Mldm13, A.Mldm14, A.Mldm15, ' +                               
               'A.Mldm16, A.Mldm17, A.Mldm18, A.Mldm19, A.Mldm20, ' +                               
               'Sminf3 ' +                                                                          
                ' From Opf' + %Trim(Pcia) + '/Lmllst01 A left join ' +                              
                'Opcus' + %Trim(Pcia) + '/Vtmotof C on A.MLCSTÑ=C.OTOCED ' +                        
                ' left join votrea' + %Trim(Pcia) + '/Vtereif D on '+                               
                'A.MlcstÑ = D.Reiced '+                                                             
             ' Inner Join Opf' + %Trim(Pcia) + '/Lslman01 On Smslsp = Mlzipc ' +                    
               'Where (A.Mlomed = ' + '''' + %Trim(Pcampa) + '''' + ')'+                            
               ' And (A.Mldm19 = ' + '''' + %Trim(Wdemo1) + '''' + ' Or ' +                         
               'A.Mldm19 = ' + '''' + %Trim(Wdemo2) + '''' + ' Or ' +                               
               'A.Mldm19 = ' + '''' + %Trim(Wdemo3) + '''' + ')' +                                  
               ' And ' + %Trim(SqlNr2) +                                                            
               ' Order By mlprlv, Mldm19, MlcstÑ';                                                  
        Else;                                                                                       
        SqlNr1 = 'Insert Into ' + %Trim(User) + '/Vtwisdf (Isddel, Isdcia, -                        
                  Isdzon, Isdced, Isdcdl, -                                                         
                  Isdpos, Isdnom, Isdcus, Isddmg, Isdddr, Isddrd, Isdciu, -                         
                 Isdcrt, Isdcor, Isdcbj, Isdest, ' +                                                
                'Isddm1, Isddm2, Isddm3, Isddm4, ' +                                                
                'Isddm5, Isddm6, Isddm7, Isddm8, ' +                                                
                'Isddm9, Isdd10, Isdd11, Isdd12, ' +                                                
                'Isdd13, Isdd14, Isdd15, Isdd16, ' +                                                
                'Isdd17, Isdd18, Isdd19, Isdd20, Isdmpl) ' +                                        
                 'Select A.Mldele, '  + '''' + Pcia + '''' + ', A.Mlzipc, -                         
                 A.MlcstÑ, -                                                                        
                 A.Mltfax, -                                                                        
                 A.Mlpost, A.Mlname, A.Mlprlv, -                                                    
                 A.Mldm19, C.Otodir, C.Otodid, A.Mlcity, A.Mlcrlm, -                                
                  A.Mlomed, Substr(Mlinf1, 7, 4) As BajaCampa' +                                    
               ', Ifnull(D.Reiesc,' + '''' + WBlan + '''' +') As CO, ' +                            
               'A.Mldm01, A.Mldm02, A.Mldm03, A.Mldm04, A.Mldm05, ' +                               
               'A.Mldm06, A.Mldm07, A.Mldm08, A.Mldm09, A.Mldm10, ' +                               
               'A.Mldm11, A.Mldm12, A.Mldm13, A.Mldm14, A.Mldm15, ' +                               
               'A.Mldm16, A.Mldm17, A.Mldm18, A.Mldm19, A.Mldm20, ' +                               
               'Sminf3 ' +                                                                          
                ' From Opf' + %Trim(Pcia) + '/Lmllst01 A left join ' +                              
                'Opcus' + %Trim(Pcia) + '/Vtmotof C on A.MLCSTÑ=C.OTOCED ' +                        
                ' left join votrea' + %Trim(Pcia) + '/Vtereif D on '+                               
                'A.MlcstÑ = D.Reiced '+                                                             
             ' Inner Join Opf' + %Trim(Pcia) + '/Lslman01 On Smslsp = Mlzipc ' +                    
               'Where (A.Mlomed = ' + '''' + %Trim(Pcampa) + '''' + ')'+                            
               ' And (A.Mldm19 = ' + '''' + %Trim(Wdemo1) + '''' + ' Or ' +                         
               'A.Mldm19 = ' + '''' + %Trim(Wdemo2) + '''' + ' Or ' +                               
               'A.Mldm19 = ' + '''' + %Trim(Wdemo3) + '''' + ')' +                                  
               ' And ( Mlzipc <> ' + '''' + 'WEB' + '''' + ')'   +                                  
               ' Order By mlprlv, Mldm19, MlcstÑ';                                                  
        Endif;                                                                                      
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur1 From :SqlNr1;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Execute Cur1;                                                                              
      /End-Free                                                                                     
     PInsertarFile     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra Archivo de Trabajo por Compañia          //                       
       //------------------------------------------------------------------//                       
     PBorrarFile       B                                                                            
     DBorrarFile       Pi                                                                           
      /Free                                                                                         
       SqlNr1 = *Blanks;                                                                            
       SqlNr1 = 'Delete From ' +  %Trim(User) + '/Vtwisdf ';                                        
                                                                                                    
        Exec Sql                                                                                    
         Prepare Cur3 From :SqlNr1;                                                                 
                                                                                                    
        Exec Sql                                                                                    
         Execute Cur3;                                                                              
      /End-Free                                                                                     
     PBorrarFile       E                                                                            
                                                                                                    
