***************************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCC24R                                         *                         
     ** Descripciòn:  : Store procedure Informar sobre la solicitud       *                         
     ** Proyecto      : Estado de Cuenta                                  *                         
     ** Autor         : John William Palacio Personalsoft                 *                         
     ** Fecha Creaciòn: 01 de Abrir de 2016                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Descripciòn:  : Se verifica que en el archivo de resumen contenga *                         
     **                 información para estudio de crédito. (VTWRI||Zona)*                         
     ** Autor         : Leonel Mauricio Parra Suárez - PersonalSoft       *                         
     ** Fecha Creaciòn: 01 de Abrir de 2016                               *                         
     **                                                                   *                         
     **********************************************************************                         
0718a** Descripciòn:  : Se agrega tabla para insertar registro con     ga *                         
0718a**                 datos de parámetros para mostrar tablas        na)*                         
0718a** Autor         : Alcides J. Rodriguez M. - PersonalSoft            *                         
0718a** Fecha Creaciòn: 09 de Julio de 2018                               *                         
0718a** 0718a                                                             *                         
     **********************************************************************                         
0219a** Descripciòn:  : Se agrega lectura de archivo portal de lideres    *                         
0219a**                 para evaluación estudio crédito por cédula        *                         
0219a** Autor         : Alcides J. Rodriguez M. - PersonalSoft            *                         
0219a** Fecha Creaciòn: 27 de Noviembre de 2018                           *                         
0219a** 0718a                                                             *                         
     **********************************************************************                         
                                                                                                    
     ** Definición de Directivas de Compilación                                                     
     HDFTACTGRP(*NO)                                                                                
     HOption(*NoDebugIO)                                                                            
                                                                                                    
     **Definiciòn de Archivos de Trabajo                                                            
                                                                                                    
     ** Definiciòn de Variables de Trabajo                                                          
     D$COMA            C                   ''''                                                     
     DWsql             S           2000A                                                            
     DW_MlpRlv         S              3a                                                            
     DW_RCDCU1         S              3A                                                            
     DW_RCDCU2         S              3A                                                            
     DW_Ced            S              9  0                                                          
     DW_Zona           S              3                                                             
     DW_Count          S              9  0                                                          
0219aDW_CountICed      S              9  0                                                          
0219aDW_CountLCed      S              9  0                                                          
0219aDW_CountL         S              9  0                                                          
0219aDW_CountR         S              9  0                                                          
0718aDW_CASVLR1        S             15  2                                                          
0718aDW_POCVCA         S             15  2                                                          
0718aDW_PindCast       S              1                                                             
0718aDW_PindTab        S              1                                                             
0718aDW_Fecha          S             10                                                             
0718aDW_Hora           S              8                                                             
                                                                                                    
     ** Definición de Estructuras de Trabajo                                                        
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3a                                                            
     DCedula                          9A                                                            
     DPInd                            1A                                                            
                                                                                                    
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DPcia                            3a                                                            
     DCedula                          9A                                                            
     DPInd                            1A                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       If Cedula <> *Blanks;                                                                        
         W_Ced = %Int(Cedula);                                                                      
         Wsql = 'Select MlpRlv, MlZipc ' +                                                          
                'From Opf' + %Trim(PCia) + '/Lmllst01 ' +                                           
                'Where MlcStÑ = ' + %Char(W_Ced);                                                   
                                                                                                    
         Exec Sql                                                                                   
           Prepare C1 From :Wsql;                                                                   
                                                                                                    
         Exec Sql                                                                                   
           Declare C1 Cursor For C1;                                                                
                                                                                                    
         Exec Sql                                                                                   
           Open C1;                                                                                 
                                                                                                    
         Exec Sql                                                                                   
           fetch C1 Into :W_MlpRlv, :W_Zona;                                                        
                                                                                                    
                                                                                                    
         If sqlCode = *Zeros;                                                                       
           Wsql = 'Select RCDCU1, RCDCU2 ' +                                                        
                  'From Votrea00/VTMRCAF ' +                                                        
                  'Where RcaCia = ' + '''' + PCia + '''';                                           
                                                                                                    
           Exec Sql                                                                                 
             Prepare C2 From :Wsql;                                                                 
                                                                                                    
           Exec Sql                                                                                 
             Declare C2 Scroll Cursor For C2;                                                       
                                                                                                    
           Exec Sql                                                                                 
             Open C2;                                                                               
                                                                                                    
           Exec Sql                                                                                 
             fetch C2 Into :W_RCDCU1, :W_RCDCU2;                                                    
                                                                                                    
           If (W_MlpRlv = W_RCDCU1) Or (W_MlpRlv =  W_RCDCU2);                                      
             PInd = 'N';                                                                            
           Else;                                                                                    
0219a      // Se evalúa portal de líderes                                                           
0219a        // Se evalúa que la cédula haya sido procesada en                                      
0219a        // portal de líderes                                                                   
0219a        Clear Wsql;                                                                            
0219a        Clear W_CountLCed;                                                                     
0219a        Wsql = 'Select Count(0) ' +                                                            
0219a               ' From Votrea'+Pcia+'/VTWRL'+W_Zona+                                            
0219a               ' Where tipotra = '+ $COMA +'Compradora'+ $COMA +                               
0219a               ' And Cedula = ' + %Char(W_Ced);                                                
0219a        Exec Sql                                                                               
0219a          Prepare C3Cedlid From :Wsql;                                                         
0219a        Exec Sql                                                                               
0219a          Declare C3CedLid Scroll Cursor For C3CedLid;                                         
0219a        Exec Sql                                                                               
0219a          Open C3CedLid;                                                                       
0219a        Exec Sql                                                                               
0219a          fetch C3CedLid Into :W_CountLCed;                                                    
0219a        Exec Sql                                                                               
0219a          Close C3CedLid;                                                                      
0219a        If SqlCod = 0;                                                                         
0219a          If W_CountLCed > 0;                                                                  
0219a            // Se evalúa que tenga estudio de crédito                                          
0219a            Clear Wsql;                                                                        
0219a            Clear W_CountL;                                                                    
0219a            Wsql = 'Select Count(0) ' +                                                        
0219a               ' From Votrea'+Pcia+'/VTWRL'+W_Zona+                                            
0219a               ' Where tipotra = '+ $COMA +'EstudioCredito'+$COMA;                             
0219a            Exec Sql                                                                           
0219a              Prepare C3Lid From :Wsql;                                                        
0219a            Exec Sql                                                                           
0219a              Declare C3Lid Scroll Cursor For C3Lid;                                           
0219a            Exec Sql                                                                           
0219a              Open C3Lid;                                                                      
0219a            Exec Sql                                                                           
0219a              fetch C3Lid Into :W_CountL;                                                      
0219a            //Se verifica que en el archivo de resumen contenga información                    
0219a            //para estudio de crédito. (VTWRL||Zona)                                           
0219a            If W_CountL > *Zeros;                                                              
0219a              PInd = 'S';                                                                      
0219a            Else;                                                                              
0219a              PInd = 'N';                                                                      
0219a            EndIf;                                                                             
0219a            Exec Sql                                                                           
0219a              Close C3Lid;                                                                     
0219a          Else;                                                                                
0219a            // Se evalúa que la cédula haya sido procesada en                                  
0219a            // portal integral                                                                 
0219a            Clear Wsql;                                                                        
0219a            Clear W_CountICed;                                                                 
0219a            Wsql = 'Select Count(0) ' +                                                        
0219a                   ' From Votrea'+Pcia+'/VTWRI'+W_Zona+                                        
0219a                   ' Where tipotra = '+ $COMA +'Compradora'+ $COMA +                           
0219a                   ' And Cedula = ' + %Char(W_Ced);                                            
0219a            Exec Sql                                                                           
0219a              Prepare C3CedInt From :Wsql;                                                     
0219a            Exec Sql                                                                           
0219a              Declare C3CedInt Scroll Cursor For C3CedInt;                                     
0219a            Exec Sql                                                                           
0219a              Open C3CedInt;                                                                   
0219a            Exec Sql                                                                           
0219a              fetch C3CedInt Into :W_CountICed;                                                
0219a            Exec Sql                                                                           
0219a              Close C3CedInt;                                                                  
0219a            If SqlCod = 0;                                                                     
0219a              If W_CountICed > 0;                                                              
                 //Se verifica que en el archivo de resumen contenga información                    
                 //para estudio de crédito. (VTWRI||Zona)                                           
                     Clear W_Count;                                                                 
                     Wsql = 'Select Count(0) ' +                                                    
0219a                       ' From Votrea'+Pcia+'/VTWRI'+W_Zona+                                    
                            ' Where tipotra = '+ $COMA +'EstudioCredito'+$COMA;                     
                     Exec Sql                                                                       
                       Prepare C3 From :Wsql;                                                       
                     Exec Sql                                                                       
                       Declare C3 Scroll Cursor For C3;                                             
                     Exec Sql                                                                       
                       Open C3;                                                                     
                     Exec Sql                                                                       
                       fetch C3 Into :W_Count;                                                      
              //Se verifica que en el archivo de resumen contenga información                       
              //para estudio de crédito. (VTWRI||Zona)                                              
                     If W_Count > *Zeros;                                                           
                       PInd = 'S';                                                                  
                     Else;                                                                          
                       PInd = 'N';                                                                  
                     EndIf;                                                                         
                     Exec Sql                                                                       
                       Close C3;                                                                    
                     Else;                                                                          
                       PInd = 'N';                                                                  
                   EndIf;                                                                           
0219a            EndIf;                                                                             
0219a          EndIf;                                                                               
0219a        EndIf;                                                                                 
           EndIf;                                                                                   
         Else;                                                                                      
           PInd = 'S';                                                                              
         EndIF;                                                                                     
         Exec Sql                                                                                   
           Close C2;                                                                                
                                                                                                    
         Exec Sql                                                                                   
           Close C1;                                                                                
                                                                                                    
0718a // Ajuste Temporal 0718a                                                                      
0718a // Evalúa condiciones                                                                         
0219a // Evalúa si el proceso fue generado en portal integral o portal                              
0219a // líderes                                                                                    
0219a    Clear W_CountR;                                                                            
0219a    Clear Wsql;                                                                                
0219a    Wsql = 'Select Count(*) ' +                                                                
0219a           ' From Votrea'+Pcia+'/VTWRL'+W_Zona+                                                
0219a           ' Where TIPOTRA = '+ $COMA +'Compradora'+ $COMA +                                   
0219a           ' And Cedula = ' + %Char(W_Ced);                                                    
0219a    Exec Sql                                                                                   
0219a      Prepare C5 From :Wsql;                                                                   
0219a    Exec Sql                                                                                   
0219a      Declare C5  Cursor For C5;                                                               
0219a    Exec Sql                                                                                   
0219a      Open C5;                                                                                 
0219a    Exec Sql                                                                                   
0219a      fetch C5 Into :W_CountR;                                                                 
0219a    If SqlCod = 0 And W_CountR > 0;                                                            
0219a      Wsql = 'Select CasVlr, CasPag ' +                                                        
0219a             ' From Votrea'+Pcia+'/VTWRL'+W_Zona+                                              
0219a             ' Where Trim(TIPOTRA) Like '+                                                     
0219a             $COMA +'%CASTIGOSINESTUDIO%'+$COMA ;                                              
0219a      Exec Sql                                                                                 
0219a        Prepare C4 From :Wsql;                                                                 
0219a      Exec Sql                                                                                 
0219a        Declare C4  Cursor For C4;                                                             
0219a      Exec Sql                                                                                 
0219a        Open C4;                                                                               
0219a      Exec Sql                                                                                 
0219a        fetch C4 Into :W_CASVLR1, :W_POCVCA;                                                   
                                                                                                    
0219a      If SqlCod = 0;                                                                           
0219a      //Evaluación 1                                                                           
0219a        Select;                                                                                
0219a          When W_CASVLR1 < W_POCVCA;                                                           
0219a            W_PindCast = 'N';                                                                  
0219a          When W_CASVLR1 = 0 And Pind = 'S';                                                   
0219a            W_PindCast = 'S';                                                                  
0219a          When  W_CASVLR1 = 0 And Pind = 'N';                                                  
0219a            W_PindCast = 'N';                                                                  
0219a          When  W_CASVLR1 <> 0 And Pind <> 'N';                                                
0219a            W_PindCast = 'S';                                                                  
0219a        EndSl;                                                                                 
                                                                                                    
0219a      //Evaluación 2                                                                           
0219a        Select;                                                                                
0219a          When Pind = 'N' And W_PindCast = 'N';                                                
0219a            W_PindTab = '2';                                                                   
0219a          When Pind = 'N' And W_PindCast = 'S';                                                
0219a            W_PindTab = '2';                                                                   
0219a          When Pind = 'S' And W_PindCast = 'S';                                                
0219a            W_PindTab = 'T';                                                                   
0219a          When Pind = 'S' And W_PindCast = 'N';                                                
0219a            W_PindTab = '3';                                                                   
0219a        EndSl;                                                                                 
0219a      EndIf;                                                                                   
0219a    Else;                                                                                      
0718a      Clear W_CountR;                                                                          
0718a      Clear Wsql;                                                                              
0219a      Wsql = 'Select Count(*) ' +                                                              
0219a             ' From Votrea'+Pcia+'/VTWRI'+W_Zona+                                              
0219a             ' Where TIPOTRA = '+ $COMA +'Compradora'+ $COMA +                                 
0219a             ' And Cedula = ' + %Char(W_Ced);                                                  
0718a      Exec Sql                                                                                 
0718a        Prepare C6 From :Wsql;                                                                 
0718a      Exec Sql                                                                                 
0718a        Declare C6  Cursor For C6;                                                             
0718a      Exec Sql                                                                                 
0718a        Open C6;                                                                               
0718a      Exec Sql                                                                                 
0718a        fetch C6 Into :W_CountR;                                                               
0718a      If W_CountR > 0;                                                                         
0718a        Wsql = 'Select CasVlr, CasPag ' +                                                      
0219a               ' From Votrea'+Pcia+'/VTWRI'+W_Zona+                                            
0718a               ' Where Trim(TIPOTRA) Like '+                                                   
0718a                 $COMA +'%CASTIGOSINESTUDIO%'+$COMA ;                                          
0718a        Exec Sql                                                                               
0718a          Prepare C7 From :Wsql;                                                               
0718a        Exec Sql                                                                               
0718a          Declare C7  Cursor For C7;                                                           
0718a        Exec Sql                                                                               
0718a          Open C7;                                                                             
0718a        Exec Sql                                                                               
0718a          fetch C7 Into :W_CASVLR1, :W_POCVCA;                                                 
0718a        If SqlCod = 0;                                                                         
0718a        //Evaluación 1                                                                         
0718a          Select;                                                                              
0718a            When W_CASVLR1 < W_POCVCA;                                                         
0718a              W_PindCast = 'N';                                                                
0718a            When W_CASVLR1 = 0 And Pind = 'S';                                                 
0718a              W_PindCast = 'S';                                                                
0718a            When  W_CASVLR1 = 0 And Pind = 'N';                                                
0718a              W_PindCast = 'N';                                                                
0718a            When  W_CASVLR1 <> 0 And Pind <> 'N';                                              
0718a              W_PindCast = 'S';                                                                
0718a          EndSl;                                                                               
                                                                                                    
0718a        //Evaluación 2                                                                         
0718a          Select;                                                                              
0718a            When Pind = 'N' And W_PindCast = 'N';                                              
0718a              W_PindTab = '2';                                                                 
0718a            When Pind = 'N' And W_PindCast = 'S';                                              
0718a              W_PindTab = '2';                                                                 
0718a            When Pind = 'S' And W_PindCast = 'S';                                              
0718a              W_PindTab = 'T';                                                                 
0718a            When Pind = 'S' And W_PindCast = 'N';                                              
0718a              W_PindTab = '3';                                                                 
0718a          EndSl;                                                                               
0219a        EndIf;                                                                                 
           Else;                                                                                    
0219a        Select;                                                                                
0219a          When W_CASVLR1 < W_POCVCA;                                                           
0219a            W_PindCast = 'N';                                                                  
0219a          When W_CASVLR1 = 0 And Pind = 'S';                                                   
0219a            W_PindCast = 'S';                                                                  
0219a          When  W_CASVLR1 = 0 And Pind = 'N';                                                  
0219a            W_PindCast = 'N';                                                                  
0219a          When  W_CASVLR1 <> 0 And Pind <> 'N';                                                
0219a            W_PindCast = 'S';                                                                  
0219a        EndSl;                                                                                 
0718a        Select;                                                                                
0718a          When Pind = 'N' And W_PindCast = 'N';                                                
0718a            W_PindTab = '2';                                                                   
0718a          When Pind = 'N' And W_PindCast = 'S';                                                
0718a            W_PindTab = '2';                                                                   
0718a          When Pind = 'S' And W_PindCast = 'S';                                                
0718a            W_PindTab = 'T';                                                                   
0718a          When Pind = 'S' And W_PindCast = 'N';                                                
0718a            W_PindTab = '3';                                                                   
0718a        EndSl;                                                                                 
0219a      EndIf;                                                                                   
0718a    EndIf;                                                                                     
0718a    W_Fecha = %Char(%Date());                                                                  
0718a    W_Hora = %Char(%Time());                                                                   
0718a    Exec Sql                                                                                   
0718a      Insert Into VotreA00/VTMPINTA                                                            
0718a             (CIAPIN, ZONPIN, CEDPIN, VLCPIN, PVLPIN,                                          
0718a              INDPIN, INCPIN, INDTAB, FECPIN, HORPIN)                                          
0718a             Values                                                                            
0718a             (:Pcia, :W_Zona, :Cedula, :W_CASVLR1, :W_POCVCA,                                  
0718a             :Pind, :W_PindCast, :W_PindTab, :W_Fecha, :W_Hora);                               
         Exec Sql                                                                                   
           Close C4;                                                                                
         Exec Sql                                                                                   
           Close C5;                                                                                
         Exec Sql                                                                                   
           Close C6;                                                                                
         Exec Sql                                                                                   
           Close C7;                                                                                
0718a  // Fin Ajuste Temporal 0718a                                                                 
                                                                                                    
       EndIf;                                                                                       
       *Inlr = *On;                                                                                 
      /End-Free                                                                                     
                                                                                                    
