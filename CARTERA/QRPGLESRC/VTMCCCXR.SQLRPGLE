     **********************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCXR                                        *                           
     ** Descripciòn:  : Visualizar Pedidos Embarcados                     *                         
     ** Autor         : Jose Luis Rodriguez Osorno (Personal Soft)        *                         
     ** Fecha Creaciòn: 09 de Junio de 2011                               *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     HDatFmt(*YMD)                                                                                  
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVTCPEI0P  Cf   e             Workstn                                                          
                                                                                                    
     **Definición de variables de trabajo                                                           
     DWvari            s            200a                                                            
     Dcmd              s           3000a                                                            
     DvalAMD           s             20                                                             
     Destado           s             20                                                             
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     Dzona                            3                                                             
     Dtransport                      22                                                             
     Dorden                           9                                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DBorrarfile       Pr                                                                           
     DProcedimientoPrincipal...                                                                     
     D                 Pr                                                                           
     DconsultarPed     Pr                                                                           
                                                                                                    
     DInsertar         Pr                                                                           
     DIzona                           3                                                             
     DItransport                     22                                                             
     DIorden                          9                                                             
     DvalidarAMD       Pr                                                                           
                                                                                                    
     DQcmdexc          Pr                  Extpgm('QCMDEXC')                                        
     D                             3000a   Const Options(*Varsize)                                  
     D                               15p 5 Const                                                    
     D                                3a   Const Options(*Nopass)                                   
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DPcia                            3                                                             
                                                                                                    
     DMain             Pi                                                                           
     DPcia                            3                                                             
                                                                                                    
                                                                                                    
       //------------------------------------------------------------------//                       
       //              P r o g r a m a   P r i n c i p a l                 //                       
       //------------------------------------------------------------------//                       
      /Free                                                                                         
       clear NUEVODISPO;                                                                            
       Dow *in03=*off;                                                                              
         Exfmt NUEVODISPO;                                                                          
         if *in03=*on;                                                                              
           iter;                                                                                    
         ENDIF;                                                                                     
         if *in12=*on;                                                                              
           clear NUEVODISPO;                                                                        
         Else;                                                                                      
           if Pestado<>*Blanks and Pano<>*Zeros and Pmes<>*Zeros and                                
             Pdia<>*Zeros;                                                                          
             validarAMD();                                                                          
             if valAMD=*Blanks;                                                                     
              select;                                                                               
                 when Pestado='1';                                                                  
                   estado='Embarcado';                                                              
                   ProcedimientoPrincipal();                                                        
                 when Pestado='2';                                                                  
                   estado='Pendiente';                                                              
                   ProcedimientoPrincipal();                                                        
                 when Pestado='3';                                                                  
                   estado='Entregado';                                                              
                   ProcedimientoPrincipal();                                                        
                 when Pestado='4';                                                                  
                   estado='Desembarcado';                                                           
                   ProcedimientoPrincipal();                                                        
                 Other;                                                                             
                  varerror='            Estado Incorrecto          ';                               
                  Exfmt error;                                                                      
              EndSL;                                                                                
             Else;                                                                                  
              varerror=' El '+valAMD+' Es Incorrecto';                                              
               Exfmt error;                                                                         
             EndIf;                                                                                 
           Else;                                                                                    
              varerror='     Ingrese La Información Faltante     ';                                 
               Exfmt error;                                                                         
           EndIf;                                                                                   
         EndIf;                                                                                     
       ENDDO;                                                                                       
        *Inlr = *On;                                                                                
      /End-Free                                                                                     
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Principal                                          //                       
       //------------------------------------------------------------------//                       
     PProcedimientoPrincipal...                                                                     
     P                 B                                                                            
     DProcedimientoPrincipal...                                                                     
     D                 Pi                                                                           
      /Free                                                                                         
         Borrarfile();                                                                              
         consultarped();                                                                            
      /End-Free                                                                                     
     PProcedimientoPrincipal...                                                                     
     P                 E                                                                            
       //------------------------------------------------------------------//                       
       // Procedimiento para validar el año el mes y el dia                //                       
       //------------------------------------------------------------------//                       
     PvalidarAMD       B                                                                            
     DvalidarAMD       Pi                                                                           
      /Free                                                                                         
       valAMD=*Blanks;                                                                              
       if Pano<1900 or Pano>%int(*Year);                                                            
          valAMD=%trim(valAMD)+'-Año-';                                                             
       EndIf;                                                                                       
       if Pmes<0 or Pmes>12;                                                                        
          valAMD=%trim(valAMD)+'-Mes-';                                                             
       EndIf;                                                                                       
       if Pdia<0 or Pdia>31;                                                                        
          valAMD=%trim(valAMD)+'-Dia-';                                                             
       EndIf;                                                                                       
      /End-Free                                                                                     
     PvalidarAMD       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que realiza la consulta principal(Dispon)          //                       
       //------------------------------------------------------------------//                       
     Pconsultarped     B                                                                            
     Dconsultarped     Pi                                                                           
     DIorden           s              9                                                             
     Dsumuni           s              9  0     Inz(*Zeros)                                          
     Dsumttal          s              9  0     Inz(*Zeros)                                          
      /Free                                                                                         
                                                                                                    
        Exec SQL                                                                                    
        Declare CU cursor for                                                                       
          SELECT DISTINCT(EMBZON), EMBTRA, ''                                                       
               FROM VTEEMBF                                                                         
               WHERE EMBEST=:estado                                                                 
                    AND    EMBANO=:Pano                                                             
                    AND    EMBMES=:Pmes                                                             
                    AND    EMBDIA=:Pdia                                                             
                    AND    EMBTRA<>' '                                                              
                    Order By EMBZON;                                                                
        Exec Sql                                                                                    
          Open CU;                                                                                  
        Exec Sql                                                                                    
          Fetch CU into :Fields;                                                                    
                                                                                                    
         Dow Sqlcod <> 100;                                                                         
           //write ejecutando;                                                                      
           Iorden=orden;                                                                            
           Insertar(zona:transport:Iorden);                                                         
                                                                                                    
             Exec SQL                                                                               
               SELECT COUNT(EMBORD) into :sumuni                                                    
                    FROM VTEEMBF                                                                    
                    WHERE EMBZON=:Zona                                                              
                    AND   EMBEST=:estado                                                            
                    AND    EMBANO=:Pano                                                             
                    AND    EMBMES=:Pmes                                                             
                    AND    EMBDIA=:Pdia                                                             
                    AND    EMBTRA=:transport;                                                       
                                                                                                    
           Zona='';                                                                                 
           Transport='  CUENTA  '+%trim(%Char(sumuni));                                             
           Iorden='';                                                                               
           Insertar(zona:transport:Iorden);                                                         
                                                                                                    
           Zona='';                                                                                 
           Transport='';                                                                            
           Iorden='';                                                                               
           Insertar(zona:transport:Iorden);                                                         
                                                                                                    
           sumttal=sumttal+sumuni;                                                                  
           sumuni=0;                                                                                
                                                                                                    
           Exec Sql                                                                                 
             Fetch CU into :Fields;                                                                 
         EndDo;                                                                                     
                                                                                                    
           Zona='';                                                                                 
           Transport='';                                                                            
           Iorden='';                                                                               
           Insertar(zona:transport:Iorden);                                                         
                                                                                                    
           Zona='';                                                                                 
           Transport='  TOTALES FINALES';                                                           
           Iorden='';                                                                               
           Insertar(zona:transport:Iorden);                                                         
                                                                                                    
                                                                                                    
           Zona='';                                                                                 
           Transport='  CUENTA  '+%Trim(%Char(sumttal));                                            
           Iorden='';                                                                               
           Insertar(zona:transport:Iorden);                                                         
                                                                                                    
        Exec Sql                                                                                    
          Close CU;                                                                                 
                                                                                                    
        varquest1='VOTREA'+ Pcia+'/VTWPEMF';                                                        
        Exfmt correctosn;                                                                           
        if siono='Y';                                                                               
        Wvari = *Blanks;                                                                            
        Wvari = 'verdes votrea'+pcia+'/VTWPEMF';                                                    
         Cmd = %Trim(Wvari);                                                                        
         Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                          
        ENDIF;                                                                                      
        clear NUEVODISPO;                                                                           
                                                                                                    
      /End-Free                                                                                     
     Pconsultarped     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Inserta en VTWPEDF                                 //                       
       //------------------------------------------------------------------//                       
     PInsertar         B                                                                            
     DInsertar         Pi                                                                           
     DIzona                           3                                                             
     DItransport                     22                                                             
     DIorden                          9                                                             
      /Free                                                                                         
        Exec Sql                                                                                    
          insert into VTWPEMF(PEMZON,                                                               
                              PEMTRA,                                                               
                              PEMORD)values(:Izona, :Itransport, :Iorden);                          
      /End-Free                                                                                     
     PInsertar         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Archivo VTWCDBF                       //                       
       //------------------------------------------------------------------//                       
     PBorrarfile       B                                                                            
     DBorrarfile       PI                                                                           
      /Free                                                                                         
         // Borra Archivo de diponibles en bodega                                                   
         Exec SQL                                                                                   
           delete from VTWPEMF;                                                                     
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
