     **********************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCYR                                          *                         
     ** Descripciòn:  : Visualizar Pedidos Aduanados                      *                         
     ** Autor         : José Luis Rodriguez Osorno (Personal Soft)        *                         
     ** Fecha Creaciòn: 10 de Junio de 2011                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     HDatFmt(*YMD)                                                                                  
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVTCPAI0P  Cf   e             Workstn                                                          
                                                                                                    
     **Definición de variables de trabajo                                                           
     DWvari            s            200a                                                            
     Dcmd              s           3000a                                                            
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     Dzona                           10                                                             
     Dorden                           9                                                             
     Dsiglo                           2                                                             
     Danno                            2                                                             
     Dmes                             2                                                             
     Ddia                             2                                                             
     Dcodau                           3                                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DBorrarfile       Pr                                                                           
     Dconsultarpea     Pr                                                                           
     DProcedimientoPrincipal...                                                                     
     D                 Pr                                                                           
     DInsertar         Pr                                                                           
     DIzona                          10                                                             
     DIorden                          9                                                             
     DIsiglo                          2                                                             
     DIanno                           2                                                             
     DImes                            2                                                             
     DIdia                            2                                                             
     DIcodau                          3                                                             
                                                                                                    
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
         Borrarfile();                                                                              
         if *in03=*on;                                                                              
           iter;                                                                                    
         ENDIF;                                                                                     
         if *in12=*on;                                                                              
           clear NUEVODISPO;                                                                        
         Else;                                                                                      
          if Psiglo<>*Zeros and Pano<>*Zeros and Pmes<>*Zeros and Padia<>*Zeros                     
            and Pcodaud<>*Blanks and Pcampa<>*Blanks;                                               
              ProcedimientoPrincipal();                                                             
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
         consultarpea();                                                                            
      /End-Free                                                                                     
     PProcedimientoPrincipal...                                                                     
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que realiza la consulta principal(Paduan500)       //                       
       //------------------------------------------------------------------//                       
     Pconsultarpea     B                                                                            
     Dconsultarpea     Pi                                                                           
     DIcc              s              2                                                             
     DIyy              s              2                                                             
     DImm              s              2                                                             
     DIdd              s              2                                                             
     DIord             s              9                                                             
     Dsumuni           s              9  0                                                          
     Dsumtotal         s              9  0                                                          
     Dvarcom           s             10        Inz(*Blanks)                                         
     Dvarcom1          s             10        Inz(*Blanks)                                         
     DvarIns           s             10        Inz(*Blanks)                                         
     Dsw               S              9        Inz(*Blanks)                                         
      /Free                                                                                         
        Exec SQL                                                                                    
        Declare CU cursor for                                                                       
         Select MLZIPC,OAORDÑ,OAODCC,OAODYY,OAODMM,OAODDD,OATRCD                                    
               From PORAUD, PORHDR, LMLLST01                                                        
               where OAORDÑ=OHORDÑ   and OHCSTÑ=MLCSTÑ                                              
               and    OATRCD=:Pcodaud                                                               
               AND    OAODCC=:Psiglo                                                                
               AND    OAODYY=:Pano                                                                  
               AND    OAODMM=:Pmes                                                                  
               AND    OAODDD=:Padia                                                                 
               AND    OHMEDA=:Pcampa                                                                
               Order By MLZIPC;                                                                     
        Exec Sql                                                                                    
          Open CU;                                                                                  
        Exec Sql                                                                                    
          Fetch CU into :Fields;                                                                    
          varcom1=zona;                                                                             
         Dow Sqlcod <> 100;                                                                         
               write ejecutando;                                                                    
               Icc=siglo;                                                                           
               Iyy=anno;                                                                            
               Imm=mes;                                                                             
               Idd=dia;                                                                             
               Iord=orden;                                                                          
               varcom=zona;                                                                         
               if varcom1<varcom;                                                                   
                  varins='          ';                                                              
                  Icc='';                                                                           
                  Iyy='';                                                                           
                  Imm='';                                                                           
                  Idd='';                                                                           
                  codau='';                                                                         
                  sw='';                                                                            
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
                  varins='    CUENTA';                                                              
                  sw=%char(sumuni);                                                                 
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
                  sumtotal=sumtotal+sumuni;                                                         
                  sumuni=0;                                                                         
                  varcom1=zona;                                                                     
                  varins='          ';                                                              
                  sw='';                                                                            
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
               Else;                                                                                
                  //varcom=zona;                                                                    
               sumuni=sumuni+1;                                                                     
               Insertar(zona:Iord:Icc:Iyy:Imm:Idd:codau);                                           
               EndIf;                                                                               
           Exec Sql                                                                                 
             Fetch Cu into :Fields;                                                                 
         EndDo;                                                                                     
                  varins='          ';                                                              
                  Icc='';                                                                           
                  Iyy='';                                                                           
                  Imm='';                                                                           
                  Idd='';                                                                           
                  codau='';                                                                         
                  sw='';                                                                            
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
                  varins='    CUENTA';                                                              
                  sw=%char(sumuni);                                                                 
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
                  sumtotal=sumtotal+sumuni;                                                         
                  sumuni=0;                                                                         
                  varcom1=zona;                                                                     
                  varins='          ';                                                              
                  sw='';                                                                            
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
                  varins='   TOTALES';                                                              
                  sw='FINALES  ';                                                                   
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
                  varins='    CUENTA';                                                              
                  sw=%char(sumtotal);                                                               
                  Insertar(varins:sw:Icc:Iyy:Imm:Idd:codau);                                        
        Exec Sql                                                                                    
          Close CU;                                                                                 
                                                                                                    
        varquest1='VOTREA'+ Pcia+'/VTWPADF';                                                        
        Exfmt correctosn;                                                                           
        if siono='Y';                                                                               
        Wvari = *Blanks;                                                                            
        Wvari = 'verdes votrea'+pcia+'/VTWPADF';                                                    
         Cmd = %Trim(Wvari);                                                                        
         Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                          
        ENDIF;                                                                                      
        clear NUEVODISPO;                                                                           
                                                                                                    
      /End-Free                                                                                     
     Pconsultarpea     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Inserta en VTWCDBF                                 //                       
       //------------------------------------------------------------------//                       
     PInsertar         B                                                                            
     DInsertar         Pi                                                                           
     DIzona                          10                                                             
     DIorden                          9                                                             
     DIsiglo                          2                                                             
     DIanno                           2                                                             
     DImes                            2                                                             
     DIdia                            2                                                             
     DIcodau                          3                                                             
      /Free                                                                                         
        Exec Sql                                                                                    
          insert into VTWPADf(PADZON,                                                               
                              PADORD,                                                               
                              PADSIG,                                                               
                              PADANO,                                                               
                              PADMES,                                                               
                              PADDIA,                                                               
                              PADCAU)                                                               
                               values(:Izona, :Iorden, :Isiglo,                                     
                                             :Ianno, :Imes, :Idia,                                  
                                             :Icodau);                                              
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
           delete from VTWPADF;                                                                     
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
                                                                                                    
                                                                                                    
