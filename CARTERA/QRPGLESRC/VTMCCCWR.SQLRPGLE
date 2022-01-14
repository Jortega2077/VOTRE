      ***********************************************************************                       
      ** Programa      : vtmcccwr                                          **                       
      ** Descripciòn   : Mto Tipos de Novedad Ajustes cartera              **                       
      ** Autor         : Jose Luis Rodriguez Osorno - PersonalSoft S.A.    **                       
      ** Fecha Creación: 25 de Julio de 2012                               **                       
      ***********************************************************************                       
     HDftactgrp(*No)                                                                                
     HOption(*NoDebugIo)                                                                            
     Hdatfmt(*Iso)                                                                                  
                                                                                                    
     ** Definiciòn de Archivo de Trabajo                                                            
     FVTMTPNF   Uf A e           k Disk                                                             
                                                                                                    
     Fvtacci1p  Cf   e             Workstn                                                          
     F                                     Sfile(Datos1:Nrr)                                        
                                                                                                    
     DPosix            s              4  0 Inz(0)                                                   
     Dmax              s              5  0 Inz(0)                                                   
     DUser             s             10a   Inz(*User)                                               
                                                                                                    
     DEsestruct        Ds                                                                           
     Dciaes                           3                                                             
     Dcodes                           3  0                                                          
     Ddeses                          40                                                             
     Dabres                           4                                                             
     Destes                           1                                                             
                                                                                                    
      *Definición de Procedimientos                                                                 
     DPintarfile       Pr                                                                           
     D Modificar       PR                                                                           
     D Crear           PR                                                                           
     Dprocedimientoppal...                                                                          
     D                 Pr                                                                           
     Dborrarfile       Pr                                                                           
     Deliminar         PR                                                                           
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DWcia                            3                                                             
     DWpais                          20                                                             
                                                                                                    
     D*--------------------------------------------------                                           
     D* Procedure name: calcularcodigo                                                              
     D* Purpose:        Calcular el Proximo codigo del tipo                                         
     D* Returns:                                                                                    
     D*--------------------------------------------------                                           
     D calcularcodigo  PR             3  0                                                          
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     Dciaentry                        3                                                             
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     Dciaentry                        3                                                             
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
                                                                                                    
      /free                                                                                         
        PgmPais(ciaentry:pdespais);                                                                 
        procedimientoppal();                                                                        
        *inlr=*on;                                                                                  
      /End-Free                                                                                     
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //        Procedimiento que realiza las acciones principales        //                        
      //////////////////////////////////////////////////////////////////////                        
     Pprocedimientoppal...                                                                          
     P                 B                                                                            
     Dprocedimientoppal...                                                                          
     D                 Pi                                                                           
      /free                                                                                         
       Dow *in03=*off;                                                                              
       PfechaW = %Char(%Date());                                                                    
       If *in03 = *On;                                                                              
          leave;                                                                                    
       EndIf;                                                                                       
       If *in12=*on;                                                                                
          clear control1;                                                                           
          *in12=*off;                                                                               
          iter;                                                                                     
       EndIf;                                                                                       
       if *in06=*on;                                                                                
         crear();                                                                                   
         *in06=*off;                                                                                
         iter;                                                                                      
       ENDIF;                                                                                       
       Pintarfile();                                                                                
        If *in03=*off and Nrr > *Zeros;                                                             
            Readc Datos1;                                                                           
               Dow Not %Eof();                                                                      
                 Select;                                                                            
                   When popcion='2';                                                                
                     modificar();                                                                   
                     Posix = Posi;                                                                  
                   When popcion='4';                                                                
                     dow PRESPUESTA=*Blanks;                                                        
                        exfmt mensaje2;                                                             
                         if PRESPUESTA='S';                                                         
                            eliminar();                                                             
                         endif;                                                                     
                      enddo;                                                                        
                      PRESPUESTA=*Blanks;                                                           
                  endsl;                                                                            
                  popcion=*Blanks;                                                                  
                Update Datos1;                                                                      
                Readc Datos1;                                                                       
              Enddo;                                                                                
         EndIf;                                                                                     
        EndDo;                                                                                      
      /End-Free                                                                                     
     Pprocedimientoppal...                                                                          
     P                 E                                                                            
      //////////////////////////////////////////////////////////////////////                        
      //        Procedimiento para pintar el subfile principal            //                        
      //////////////////////////////////////////////////////////////////////                        
     PPintarfile       B                                                                            
     Dpintarfile       Pi                                                                           
     Dfecha1           s              8                                                             
      /free                                                                                         
        borrarfile();                                                                               
        Exec Sql                                                                                    
         Declare cursor1 Cursor For                                                                 
           SELECT TPNCIA, TPNCOD, TPNDES, TPNABR, TPNEST                                            
           FROM votrea00/vtmtpnf                                                                    
           where tpncia=:CiaEntry;                                                                  
                                                                                                    
        Exec Sql                                                                                    
          Open cursor1;                                                                             
                                                                                                    
        Exec Sql                                                                                    
          Fetch cursor1 into:Esestruct;                                                             
                                                                                                    
          Dow sqlcod<>100 And SqlCod >= *Zeros;                                                     
               PPCIA=ciaes;                                                                         
               PPCOD=codes;                                                                         
               PPDES=deses;                                                                         
               PPABRE=abres;                                                                        
               PPESTA=estes;                                                                        
                                                                                                    
               if estes=' ';                                                                        
                  PPDESEST='Activo';                                                                
                  *in35=*off;                                                                       
               else;                                                                                
                  PPDESEST='Inactivo';                                                              
                  *in35=*on;                                                                        
               ENDIF;                                                                               
                                                                                                    
               nrr = nrr + 1;                                                                       
               posi=nrr;                                                                            
               write datos1;                                                                        
           Exec Sql                                                                                 
             Fetch cursor1 into:Esestruct;                                                          
                                                                                                    
          EndDo;                                                                                    
                                                                                                    
        Exec Sql                                                                                    
          Close cursor1;                                                                            
                                                                                                    
       *in70 = *On;                                                                                 
        If nrr > *Zeros;                                                                            
         *in85 = *On;                                                                               
        Else;                                                                                       
         *in85 = *Off;                                                                              
        Endif;                                                                                      
                                                                                                    
        if posix > *Zeros;                                                                          
         if posix>=nrr;                                                                             
           posic=nrr;                                                                               
         else;                                                                                      
          posic=posix;                                                                              
         ENDIF;                                                                                     
        ENDIF;                                                                                      
                                                                                                    
       *in80 = *On;                                                                                 
       write rdatot2;                                                                               
       Write teclas1;                                                                               
       Exfmt control1;                                                                              
       *in80=*off;                                                                                  
                                                                                                    
      /End-Free                                                                                     
     Ppintarfile       E                                                                            
                                                                                                    
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de Borrar Archivo de Control        //                       
       //////////////////////////////////////////////////////////////////////                       
     PBorrarfile       B                                                                            
     DBorrarfile       Pi                                                                           
      /Free                                                                                         
       Posic = 1;                                                                                   
       Nrr = *Zeros;                                                                                
       *in75 = *On;                                                                                 
       Write Control1;                                                                              
       *in75 = *Off;                                                                                
      /End-Free                                                                                     
     PBorrarfile       E                                                                            
       //////////////////////////////////////////////////////////////////////                       
       // Procedimiento que se Encarga de eliminar el registro de la temp  //                       
       // Principal                                                        //                       
       //////////////////////////////////////////////////////////////////////                       
     Peliminar         B                                                                            
     Deliminar         Pi                                                                           
      /free                                                                                         
         Chain (PPCIA:PPCOD) VTMTPNF;                                                               
           if %found(VTMTPNF);                                                                      
             delete regtpn;                                                                         
           ENDIF;                                                                                   
      /end-free                                                                                     
     Peliminar         E                                                                            
                                                                                                    
                                                                                                    
     P*--------------------------------------------------                                           
     P* Procedure name: Modificar                                                                   
     P* Purpose:        modificar la fecha de ejecucion                                             
     P* Returns:                                                                                    
     P*--------------------------------------------------                                           
     P Modificar       B                                                                            
     D Modificar       PI                                                                           
      /FREE                                                                                         
        PCODIGO=PPCOD;                                                                              
        pdescrip=PPDES;                                                                             
        pabre=PPABRE;                                                                               
        pest=PPESTA;                                                                                
        dow *in03=*off;                                                                             
          exfmt mensaje5;                                                                           
          if *in03=*on;                                                                             
            leave;                                                                                  
          ENDIF;                                                                                    
          if *in12=*on;                                                                             
            PCODIGO=PPCOD;                                                                          
            pdescrip=PPDES;                                                                         
            pabre=PPABRE;                                                                           
            pest=PPESTA;                                                                            
            *in12=*off;                                                                             
            iter;                                                                                   
          ENDIF;                                                                                    
           if PDESCRIP<>*Blanks;                                                                    
             *in30=*off;                                                                            
             if PABRE<>*Blanks;                                                                     
               if Pabre = 'P' or Pabre = 'C' or Pabre = 'I' or Pabre = 'A'                          
                  or Pabre = 'P+' or Pabre = 'C+' or Pabre = 'I+'                                   
                  or Pabre = 'A+' or Pabre = 'P-' or Pabre = 'C-'                                   
                  or Pabre = 'I-' or Pabre = 'A,';                                                  
                  *in31=*off;                                                                       
                  Chain (ciaentry:PCODIGO) VTMTPNF;                                                 
                  if %found(VTMTPNF);                                                               
                    TPNDES=pdescrip;                                                                
                    TPNABR=pabre;                                                                   
                    TPNEST=pest;                                                                    
                    Update regtpn;                                                                  
                    *in61=*on;                                                                      
                     PPMSJ='     Registro Modificado Correctamente';                                
                     exfmt mensaje1;                                                                
                     *in61=*off;                                                                    
                     *in03=*on;                                                                     
                  else;                                                                             
                     PPMSJ='    El Registro no se Pudo Modificar';                                  
                     exfmt mensaje1;                                                                
                  ENDIF;                                                                            
              else;                                                                                 
               PPMSJ='      Ingrese Una Abreviatura Valida';                                        
               exfmt mensaje1;                                                                      
               *in31=*on;                                                                           
             ENDIF;                                                                                 
            else;                                                                                   
              PPMSJ= '    Ingrese la Abreviatura  del tipo';                                        
              exfmt mensaje1;                                                                       
              *in31=*on;                                                                            
            ENDIF;                                                                                  
           else;                                                                                    
             PPMSJ='    Ingrese la descripción  del tipo';                                          
             exfmt mensaje1;                                                                        
             *in30=*on;                                                                             
           ENDIF;                                                                                   
        ENDDO;                                                                                      
        *in03=*off;                                                                                 
      /END-FREE                                                                                     
     P Modificar       E                                                                            
     P*--------------------------------------------------                                           
     P* Procedure name: Crear                                                                       
     P* Purpose:        Crear la fecha de ejecucion                                                 
     P* Returns:                                                                                    
     P*--------------------------------------------------                                           
     P Crear           B                                                                            
     D Crear           PI                                                                           
      /FREE                                                                                         
        dow *in03=*off;                                                                             
          PCODIGO=calcularcodigo();                                                                 
          exfmt mensaje4;                                                                           
          if *in03=*on;                                                                             
            leave;                                                                                  
          ENDIF;                                                                                    
          if *in12=*on;                                                                             
            CLEAR PDESCRIP;                                                                         
            CLEAR PABRE;                                                                            
            CLEAR PEST;                                                                             
            *in30=*off;                                                                             
            *in31=*off;                                                                             
            iter;                                                                                   
          ENDIF;                                                                                    
           if PDESCRIP<>*Blanks;                                                                    
             *in30=*off;                                                                            
             if PABRE<>*Blanks;                                                                     
               if Pabre = 'P' or Pabre = 'C' or Pabre = 'I' or Pabre = 'A'                          
                  or Pabre = 'P+' or Pabre = 'C+' or Pabre = 'I+'                                   
                  or Pabre = 'A+' or Pabre = 'P-' or Pabre = 'C-'                                   
                  or Pabre = 'I-' or Pabre = 'A,';                                                  
                                                                                                    
                  *in31=*off;                                                                       
                  Chain (ciaentry:PCODIGO) VTMTPNF;                                                 
                  if not %found(VTMTPNF);                                                           
                    TPNCIA=ciaentry;                                                                
                    TPNCOD=pcodigo;                                                                 
                    TPNDES=pdescrip;                                                                
                    TPNABR=pabre;                                                                   
                    TPNEST=pest;                                                                    
                    write regtpn;                                                                   
                    *in61=*on;                                                                      
                     PPMSJ='     Registro Grabado Correctamente';                                   
                     exfmt mensaje1;                                                                
                     CLEAR PDESCRIP;                                                                
                     CLEAR PABRE;                                                                   
                     CLEAR PEST;                                                                    
                                                                                                    
                    *in61=*off;                                                                     
                  else;                                                                             
                     PPMSJ='    El Registro no se Pudo Insertar';                                   
                     exfmt mensaje1;                                                                
                  ENDIF;                                                                            
                else;                                                                               
                  PPMSJ='       Ingrese Una Abreviatura Valida';                                    
                  exfmt mensaje1;                                                                   
                  *in31=*on;                                                                        
                ENDIF;                                                                              
             else;                                                                                  
               PPMSJ='    Ingrese la Abreviatura  del tipo';                                        
               exfmt mensaje1;                                                                      
               *in31=*on;                                                                           
             ENDIF;                                                                                 
           else;                                                                                    
             PPMSJ='    Ingrese la descripción  del tipo';                                          
             exfmt mensaje1;                                                                        
             *in30=*on;                                                                             
           ENDIF;                                                                                   
        ENDDO;                                                                                      
        *in03=*off;                                                                                 
      /END-FREE                                                                                     
     P Crear           E                                                                            
                                                                                                    
                                                                                                    
     P*--------------------------------------------------                                           
     P* Procedure name: calcularcodigo                                                              
     P* Purpose:        Calcular el Proximo codigo del tipo                                         
     P* Returns:                                                                                    
     P*--------------------------------------------------                                           
     P calcularcodigo  B                                                                            
     D calcularcodigo  PI             3  0                                                          
     Dcodnew           S              3  0                                                          
                                                                                                    
      /FREE                                                                                         
        Exec SQL                                                                                    
         Select ifnull((max(tpncod) + 1), 1) Into :codnew                                           
         from votrea00/vtmtpnf                                                                      
         where tpncia=:CiaEntry;                                                                    
                                                                                                    
         return codnew;                                                                             
                                                                                                    
      /END-FREE                                                                                     
     P calcularcodigo  E                                                                            
                                                                                                    
