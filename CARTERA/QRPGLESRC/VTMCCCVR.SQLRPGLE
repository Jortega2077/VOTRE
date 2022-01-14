     **********************************************************************                         
     **                                                                   *                         
     ** Programa      : VTMCCCVR                                          *                         
     ** Descripciòn:  : Visualizar Disponible en bodega                   *                         
     ** Autor         : Jose Luis Rodriguez Osorno (Personal Soft)        *                         
     ** Fecha Creaciòn: 08 de Junio de 2011                               *                         
     **                                                                   *                         
     **********************************************************************                         
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
     HDatFmt(*YMD)                                                                                  
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVTCUDI0P  Cf   e             Workstn                                                          
                                                                                                    
     **Definición de variables de trabajo                                                           
     DWvari            s            200a                                                            
     Dcmd              s           3000a                                                            
                                                                                                    
     ** Definiciòn de Estructuras de Trabajo                                                        
     DFields           Ds                                                                           
     Ditem                           15                                                             
     Dvendor                         20                                                             
     Dean                            13  0                                                          
     Dqtystock                       15  3                                                          
     Dqtycomm                        15  3                                                          
     Ddispo                          16  3                                                          
     Dlocation                        8                                                             
                                                                                                    
     ** Definiciòn de Prototipos de Trabajo                                                         
     DBorrarfile       Pr                                                                           
     DProcedimientoPrincipal...                                                                     
     D                 Pr                                                                           
     Dcrearalias       Pr                                                                           
     Dconsultardis     Pr                                                                           
     DInsertar         Pr                                                                           
     DIitem                          15                                                             
     DIvendor                        20                                                             
     DIean                           13  0                                                          
     DIqtystock                      15  3                                                          
     DIqtycomm                       15  3                                                          
     DIdispo                         16  3                                                          
     DIlocation                       8                                                             
                                                                                                    
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
           if PBOD1<>*Blanks and PBOD2<>*Blanks and PSTOCK<>*Blanks;                                
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
         crearalias();                                                                              
         Borrarfile();                                                                              
         consultardis();                                                                            
      /End-Free                                                                                     
     PProcedimientoPrincipal...                                                                     
     P                 E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que realiza la consulta principal(Dispon)          //                       
       //------------------------------------------------------------------//                       
     Pconsultardis     B                                                                            
     Dconsultardis     Pi                                                                           
     DIean1            s             13  0                                                          
     DIqtystock1       s             15  3                                                          
     DIqtycomm1        s             15  3                                                          
     DIdispo1          s             16  3                                                          
     Dsum1             s             15  3                                                          
     Dsum2             s             15  3                                                          
     Dsum3             s             16  3                                                          
      /Free                                                                                         
        Exec SQL                                                                                    
        Declare CU cursor for                                                                       
          Select Ifnull(Ioitem, ''), Ifnull(Irvitm,''),                                             
                 Ifnull(Eancod, 0), Ifnull(Ioqtyo, 0),Ifnull(Iqqtym, 0),                            
                 (ifnull(Ioqtyo, 0)-ifnull(Iqqtym, 0)), Ifnull(Iolocn, '')                          
          From Linqoh01 Left Join Linvrf01 On                                                       
          Ioitem = Iritem Inner Join Linqty01 On                                                    
          Ioitem = Iqitem Inner Join Linvnt01 On                                                    
          Ioitem = Ivitem Left Join Qtemp/Opieanf3 On                                               
          Ioitem = Eanitm                                                                           
          Where Iowhse =:PBOD1 And Ivstck =:PSTOCK And                                              
          Iqwhse =:PBOD2 order by Ioitem;                                                           
                                                                                                    
        Exec Sql                                                                                    
          Open CU;                                                                                  
        Exec Sql                                                                                    
          Fetch CU into :Fields;                                                                    
                                                                                                    
         Dow Sqlcod <> 100;                                                                         
               write ejecutando;                                                                    
               Iean1=ean;                                                                           
               Iqtystock1=qtystock;                                                                 
               Iqtycomm1=qtycomm;                                                                   
               Idispo1=dispo;                                                                       
                                                                                                    
           Insertar(item:vendor:Iean1:Iqtystock1:Iqtycomm1:Idispo1:location);                       
           Exec Sql                                                                                 
             Fetch Cu into :Fields;                                                                 
         EndDo;                                                                                     
                                                                                                    
        Exec Sql                                                                                    
          Close CU;                                                                                 
                                                                                                    
         Exec SQl                                                                                   
          Select sum(Ioqtyo),sum(Iqqtym),sum(Ioqtyo-Iqqtym)                                         
              into :sum1, :sum2, :sum3                                                              
          From Linqoh01 Left Join Linvrf01 On                                                       
          Ioitem = Iritem Inner Join Linqty01 On                                                    
          Ioitem = Iqitem Inner Join Linvnt01 On                                                    
          Ioitem = Ivitem Left Join Qtemp/Opieanf3 On                                               
          Ioitem = Eanitm                                                                           
          Where Iowhse =:PBOD1 And Ivstck =:PSTOCK And                                              
          Iqwhse =:PBOD2;                                                                           
                                                                                                    
        vendor='TOTAL';                                                                             
        item='';                                                                                    
        Iean1=0;                                                                                    
        location='';                                                                                
        Insertar(item:vendor:Iean1:sum1:sum2:sum3:location);                                        
                                                                                                    
        varquest1='VOTREA'+ Pcia+'/VTWCDBF';                                                        
        Exfmt correctosn;                                                                           
        if siono='Y';                                                                               
        Wvari = *Blanks;                                                                            
        Wvari = 'verdes votrea'+pcia+'/VTWCDBF';                                                    
         Cmd = %Trim(Wvari);                                                                        
         Callp(E) Qcmdexc(Cmd:%Size(Cmd));                                                          
        ENDIF;                                                                                      
        clear NUEVODISPO;                                                                           
                                                                                                    
      /End-Free                                                                                     
     Pconsultardis     E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento Inserta en VTWCDBF                                 //                       
       //------------------------------------------------------------------//                       
     PInsertar         B                                                                            
     DInsertar         Pi                                                                           
     DIitem                          15                                                             
     DIvendor                        20                                                             
     DIean                           13  0                                                          
     DIqtystock                      15  3                                                          
     DIqtycomm                       15  3                                                          
     DIdispo                         16  3                                                          
     DIlocation                       8                                                             
      /Free                                                                                         
        Exec Sql                                                                                    
          insert into VTWCDBF(CDBITM,                                                               
                              CDBIRV,                                                               
                              CDBEAN,                                                               
                              CDBTYO,                                                               
                              CDBTYM,                                                               
                              CDBDIS,                                                               
                              CDBLOC) values(:Iitem, :Ivendor, :Iean,                               
                                             :Iqtystock, :Iqtycomm, :Idispo,                        
                                             :Ilocation);                                           
      /End-Free                                                                                     
     PInsertar         E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Crea El alias                                  //                       
       //------------------------------------------------------------------//                       
     Pcrearalias       B                                                                            
     Dcrearalias       Pi                                                                           
     DStrCmd           s            500                                                             
     Dmiembro          s             10    Inz('Cia')                                               
     D                                                                                              
      /Free                                                                                         
       Exec SQL                                                                                     
         Drop alias Qtemp/Opieanf3;                                                                 
                                                                                                    
          miembro=%trim(miembro)+%trim(Pcia);                                                       
          StrCmd = *Blanks;                                                                         
          StrCmd = 'Create Alias Qtemp/Opieanf3 ' +                                                 
             'For ORDERP00/Opieanf3 ' +                                                             
             '(' + %Trim(Miembro) + ')';                                                            
         Exec Sql                                                                                   
          Execute Immediate :StrCmd;                                                                
                                                                                                    
      /End-Free                                                                                     
     Pcrearalias       E                                                                            
                                                                                                    
       //------------------------------------------------------------------//                       
       // Procedimiento que Borra el Archivo VTWCDBF                       //                       
       //------------------------------------------------------------------//                       
     PBorrarfile       B                                                                            
     DBorrarfile       PI                                                                           
      /Free                                                                                         
         // Borra Archivo de diponibles en bodega                                                   
         Exec SQL                                                                                   
           delete from VTWCDBF;                                                                     
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
