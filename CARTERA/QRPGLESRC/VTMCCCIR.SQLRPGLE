      **********************************************************************                        
      *                                                                   **                        
      * Programa      : VTMCCCIR                                          **                        
      * Descripcion   : Generación Informes de las Genéricas Países       **                        
      * Proyecto      : O.P.I Paises                                      **                        
      * Autor         : JLOPEZL                                           **                        
      * Fecha creacion: Julio  27 del 2.009                               **                        
      *********************************************************************                         
     ** Definicion de Archivos de Trabajo                                                           
     FVtmgcof   If a e           k Disk                                                             
     FLardtl01  If   e           k Disk                                                             
                                                                                                    
     ** Definiciòn de Archivo de Pantallas                                                          
     FVtcpci1p  Cf   e             Workstn                                                          
     F                                     Sfile(Rdatos:Nrr)                                        
                                                                                                    
     ** Variables de Trabajo a Utilizar                                                             
     DNrr              s              4s 0 Inz(*Zeros)                                              
     DContar           s              6s 0 Inz(*Zeros)                                              
     DArchiv           s             10a   Inz(*Blanks)                                             
     DSqlStr           s           1000a                                                            
     DSqlStr1          s           1000a                                                            
     DSqlStr2          s           1000a                                                            
                                                                                                    
     DFecha2          sds                                                                           
     DSig_sys                195    196  0                                                          
     DUsuario                254    263                                                             
     DAÑo_sys                280    281  0                                                          
     DMes_sys                276    277  0                                                          
     DDia_sys                278    279  0                                                          
                                                                                                    
     D                 DS                                                                           
     DAÑo                      1      2S 0                                                          
     DMes                      3      4S 0                                                          
     DDia                      5      6S 0                                                          
     DFecha                    1      6S 0                                                          
                                                                                                    
     DCia              S              3    Inz(*Blanks)                                             
     DXcia             s              3    Inz(*Blanks)                                             
     DXpais            s             20    Inz(*Blanks)                                             
                                                                                                    
     DPgmPais          Pr                  Extpgm('VOTREP00/VTMCSBBR')                              
     DXcia                            3                                                             
     DXpais                          20                                                             
                                                                                                    
                                                                                                    
     **************************************************************                                 
     *** Programa principal                                      **                                 
     **************************************************************                                 
                                                                                                    
                                                                                                    
      /Free                                                                                         
          Archiv = 'VTGENER' + Compania;                                                            
          PgmPais(Compania:Xpais);                                                                  
                                                                                                    
          Dow *In03 = *Off;                                                                         
              Ppais = Xpais;                                                                        
              Write Rteclas;                                                                        
              Exsr SrSubfile3;                                                                      
                                                                                                    
              If *In03 = *On;                                                                       
                Leave;                                                                              
              Endif;                                                                                
                                                                                                    
              If *In12 = *On;                                                                       
               Clear Rdatos;                                                                        
               Iter;                                                                                
              Endif;                                                                                
                                                                                                    
              If *In06 = *On;                                                                       
                  Exsr srCrear;                                                                     
                  Exsr  SrSubfile3;                                                                 
                  Write Rteclas;                                                                    
              Endif;                                                                                
                                                                                                    
              Exsr Opcion;                                                                          
                                                                                                    
                                                                                                    
          Enddo;                                                                                    
                                                                                                    
        *inlr = *On;                                                                                
                                                                                                    
         //************************************************************//                           
         //** Subrutina LLenado SubFile                             ***//                           
         //************************************************************//                           
        Begsr SrSubfile3;                                                                           
         Exsr SrBorrar3;                                                                            
         Exsr SrLlenar3;                                                                            
                                                                                                    
         *In80 = *On;                                                                               
         Exfmt RControl;                                                                            
         *In80 = *Off;                                                                              
                                                                                                    
              If *In05 = *On and                                                                    
                  Pfecha1 <> *Zeros;                                                                
                  Exsr srBorrarFile;                                                                
                  Exsr srCrearFile;                                                                 
                  Exsr srInsertarFile;                                                              
                                                                                                    
                  Sqlstr = *Blanks;                                                                 
                  Sqlstr = 'Select Count(Gopced) As Contar ' +                                      
                     'From ' + %Trim(Usuario) + '/' + %Trim(Archiv) ;                               
                                                                                                    
                  Exec Sql                                                                          
                   Prepare Curs From :SqlStr;                                                       
                                                                                                    
                  Exec Sql                                                                          
                   Declare Curso Cursor For Curs;                                                   
                                                                                                    
                  Exec Sql                                                                          
                   Open Curso;                                                                      
                                                                                                    
                  Exec Sql                                                                          
                   Fetch Curso Into :Contar;                                                        
                                                                                                    
                  If Sqlcod =  100;                                                                 
                   Pmsg  = 'No se encontraron registros con los ' +                                 
                           'criterios de búsqueda, verifique.';                                     
                  ElseIf Contar > *Zeros;                                                           
                   Pmsg  = 'Se puede descargar el archivo ' + Archiv +                              
                             ' de la Biblioteca ' + %Trim(Usuario);                                 
                  Else;                                                                             
                   Pmsg  = 'No se encontraron registros con los ' +                                 
                           'criterios de búsqueda, verifique.';                                     
                  Endif;                                                                            
                                                                                                    
                  Exec Sql                                                                          
                   Close Curso;                                                                     
                                                                                                    
                  Clear Rcontrol;                                                                   
                  Exfmt Rdato1;                                                                     
                  Pmsg  = *Blanks;                                                                  
                  Write Rteclas;                                                                    
              Else;                                                                                 
              If *In05 = *On and                                                                    
                  (Pfecha1 = *Zeros);                                                               
               *In80 = *On;                                                                         
               Write RControl;                                                                      
               *In80 = *Off;                                                                        
               *in38 = *On;                                                                         
               Exfmt Rdato1;                                                                        
               *In38 = *Off;                                                                        
              Endif;                                                                                
             Endif;                                                                                 
                                                                                                    
        EndSr;                                                                                      
                                                                                                    
                                                                                                    
        Begsr SrBorrar3;                                                                            
         Nrr = 0;                                                                                   
         *In75 = *On;                                                                               
         Write RControl;                                                                            
         *In75 = *Off;                                                                              
        EndSr;                                                                                      
                                                                                                    
                                                                                                    
        Begsr SrLlenar3;                                                                            
                                                                                                    
            Exec Sql                                                                                
             Declare C1                                                                             
             Cursor For                                                                             
               Select Gcoced, Gcodes                                                                
               From Vtmgcof                                                                         
               Where Gcocia = :Compania                                                             
               Order By Gcoced;                                                                     
                                                                                                    
            Exec Sql                                                                                
             Open C1;                                                                               
                                                                                                    
            Exec Sql                                                                                
             Fetch C1 Into :Gcoced, :Gcodes;                                                        
               Dow Sqlcod <> 100;                                                                   
                 Nrr = Nrr + 1;                                                                     
                 Pced = Gcoced;                                                                     
                 Pnombre = Gcodes;                                                                  
                 Pop = *Blanks;                                                                     
                 Write Rdatos;                                                                      
                                                                                                    
            Exec Sql                                                                                
             Fetch C1 Into :Gcoced, :Gcodes;                                                        
               Enddo;                                                                               
                                                                                                    
            Exec Sql                                                                                
             Close C1;                                                                              
                                                                                                    
         *In40 = *On;                                                                               
         If Nrr > *Zeros;                                                                           
          *In85 = *On;                                                                              
         EndIf;                                                                                     
        EndSr;                                                                                      
                                                                                                    
        //**************Rutina Opciones********************                                         
          Begsr Opcion;                                                                             
           Readc Rdatos;                                                                            
           Dow Not %Eof();                                                                          
            Select;                                                                                 
             When Pop = '4';                                                                        
              Exsr srEliminar;                                                                      
            //Posix = Posi;                                                                         
             Endsl;                                                                                 
             Pop = *Blanks;                                                                         
             Update Rdatos;                                                                         
             Readc Rdatos;                                                                          
           Enddo;                                                                                   
          Endsr;                                                                                    
                                                                                                    
         //************************************************************//                           
         //** Rutina que elimina tabla  Vtwgopf de la libreria del  ***//                           
         //** usuario que ejecuta el proceso                        ***//                           
         //************************************************************//                           
                                                                                                    
           Begsr srBorrarFile;                                                                      
            SqlStr = *Blanks;                                                                       
            SqlStr = 'Drop Table ' + %Trim(Usuario) + '/' + %Trim(Archiv);                          
                                                                                                    
            Exec Sql                                                                                
             Prepare Cur From :SqlStr;                                                              
                                                                                                    
            Exec Sql                                                                                
             Execute Cur;                                                                           
           Endsr;                                                                                   
                                                                                                    
                                                                                                    
         //************************************************************//                           
         //** Rutina que crea    tabla  Vtwgopf de la libreria del  ***//                           
         //** usuario que ejecuta el proceso                        ***//                           
         //************************************************************//                           
                                                                                                    
           Begsr srCrearFile;                                                                       
            SqlStr = *Blanks;                                                                       
            SqlStr = 'Create Table ' + %Trim(Usuario) + '/' +                                       
                 %Trim(Archiv)+ ' (GOPCIA CHAR(3), GOPCED DECIMAL(9, 0), +                          
                      GOPORD DECIMAL(9, 0), GOPTTR CHAR(1),  +                                      
                      GOPVTR DECIMAL(11, 2), GOPCOM CHAR(20), +                                     
                      GOPFEC DECIMAL(6, 0), GOPTEN CHAR(3), +                                       
                      GOPUSR CHAR(10))';                                                            
                                                                                                    
            Exec Sql                                                                                
              Prepare Cur1 From :SqlStr;                                                            
                                                                                                    
            Exec Sql                                                                                
              Execute Cur1;                                                                         
                                                                                                    
           Endsr;                                                                                   
         //************************************************************//                           
         //** Rutina que inserta datos en la tabla creada           ***//                           
         //************************************************************//                           
           begsr srInsertarFile;                                                                    
             Cia=%Trim(Compania);                                                                   
             Exec Sql                                                                               
              Declare C2                                                                            
              Cursor For                                                                            
                                                                                                    
              Select C.ADCSTÑ, C.ADORDÑ, C.ADTRTY,C.ADTRAM, C.ADTNDÑ,                               
              (DIGITS(C.ADTRYY))||(DIGITS(C.ADTRMM))||(DIGITS(C.ADTRDD))                            
              AS FECHA, C.ADTEND, C.ADUSER  FROM Lardtl01 AS C,                                     
              (select adcstÑ, adordÑ, saldo                                                         
              from( select adcstÑ, adordÑ, sum(adtram) as saldo                                     
              from Lardtl01                                                                         
              where (((DIGITS(ADTRYY))||(DIGITS(ADTRMM))|| +                                        
                   (DIGITS(ADTRDD))) <= :Pfecha1 )                                                  
              group by adcstÑ, adordÑ order by adcstÑ, adordÑ ) as a                                
              where  a.adcstÑ in (select  gcoced from                                               
              votrea00/vtmgcof where gcocia=:Cia)) AS B                                             
             WHERE C.ADCSTÑ=B.ADCSTÑ AND C.ADORDÑ=B.ADORDÑ  AND SALDO<>0                            
             ORDER BY C.ADCSTÑ,  C.ADORDÑ ;                                                         
                                                                                                    
            Exec Sql                                                                                
             Open C2;                                                                               
                                                                                                    
            Exec Sql                                                                                
               Fetch C2 Into :AdcstÑ, :AdordÑ, :Adtrty,                                             
                        :Adtram, :AdtndÑ, :Fecha, :Adtend, :Aduser;                                 
                                                                                                    
            Dow Sqlcod <> 100;                                                                      
              SqlStr = *Blanks;                                                                     
                                                                                                    
              SqlStr = 'Insert Into ' + %Trim(Usuario) + '/' +                                      
                       %Trim(Archiv)+ ' (GOPCIA, GOPCED, GOPORD, +                                  
                       GOPTTR, GOPVTR, GOPCOM, GOPFEC, GOPTEN, +                                    
                       GOPUSR) Values('                                                             
                      + '''' + %Trim(compania) + '''' +  ', '                                       
                      + %Trim(%editc(AdcstÑ:'Z')) + ', '                                            
                      + %Trim(%editc(AdordÑ:'Z')) + ', '                                            
                      + '''' + %Trim(Adtrty) + '''' +  ', '                                         
                      + %Trim(%editc(Adtram:'P')) + ', '                                            
                      + '''' + %Trim(AdtndÑ) + '''' +  ', '                                         
                      + %Trim(%editc(Fecha:'Z')) + ', '                                             
                      + '''' + %Trim(Adtend) + '''' +  ', '                                         
                      + '''' + %Trim(Aduser) + '''' +  ')';                                         
               Exec Sql                                                                             
                 Prepare Cur2 From :SqlStr;                                                         
                                                                                                    
               Exec Sql                                                                             
                 Execute Cur2;                                                                      
                                                                                                    
            Exec Sql                                                                                
               Fetch C2 Into :AdcstÑ, :AdordÑ, :Adtrty,                                             
                        :Adtram, :AdtndÑ, :Fecha, :Adtend, :Aduser;                                 
             Enddo;                                                                                 
                                                                                                    
             Exec Sql                                                                               
              Close C2;                                                                             
                                                                                                    
           Endsr;                                                                                   
                                                                                                    
         //************************************************************//                           
         //** Crear Registro en Archivo de trabajo Cedulas Generica ***//                           
         //************************************************************//                           
          Begsr srCrear;                                                                            
             Clear Rdatos4;                                                                         
             Exfmt Rdatos4;                                                                         
               Chain(n) (Compania:Pcedula) Vtmgcof;                                                 
                 If Not %Found(Vtmgcof);                                                            
                   Gcocia = Compania;                                                               
                   Gcoced = Pcedula;                                                                
                   Gcodes = Pnomb;                                                                  
                   Write Reggco;                                                                    
                 Endif;                                                                             
             *in39 = *On;                                                                           
             Exfmt Rdato1;                                                                          
             *In39 = *Off;                                                                          
                                                                                                    
             Clear Rdatos4;                                                                         
             Exfmt Rdatos4;                                                                         
                                                                                                    
          Endsr;                                                                                    
                                                                                                    
         //************************************************************//                           
         //** Elimina Registro de la tabla vtmgcof                  ***//                           
         //************************************************************//                           
          Begsr srEliminar;                                                                         
              Exec Sql                                                                              
               Delete From Votrea00/Vtmgcof                                                         
                Where  GCOCIA = :Compania And Gcoced = :Pced;                                       
                                                                                                    
             *in36 = *On;                                                                           
             Exfmt Rdato1;                                                                          
             *In36 = *Off;                                                                          
                                                                                                    
          Endsr;                                                                                    
                                                                                                    
         //************************************************************//                           
         //** Rutina que inserta datos en la tabla creada           ***//                           
         //************************************************************//                           
           begsr srInsertarFileX;                                                                   
             SqlStr1 = *Blanks;                                                                     
             SqlStr1 = 'Select ADCSTÑ, ADORDÑ, ADTRTY, sum(ADTRAM) as +                             
                        Valor, ADTNDÑ,+                                                             
                       (DIGITS(ADTRYY))||(DIGITS(ADTRMM))||(DIGITS(ADTRDD))+                        
                       AS FECHA, ADTEND, ADUSER ' +                                                 
                       'From ' + 'OPF' + %Trim(Compania) + '/Lardtl01 ' +                           
                       'Inner Join Votrea00/Vtmgcof On AdcstÑ = Gcoced ' +                          
                       'Where (((DIGITS(ADTRYY))||(DIGITS(ADTRMM))|| +                              
                        (DIGITS(ADTRDD))) < ' + %Trim(%Editc(Pfecha1:'Z')) +                        
                        ')' +                                                                       
                       ' And Gcocia = ' + %Trim(Compania)   +                                       
                       ' Group By ADCSTÑ, ADORDÑ ' +                                                
                       ' Order by ADCSTÑ, ADORDÑ )';                                                
                                                                                                    
             SqlStr2 = *Blanks;                                                                     
             SqlStr2 = 'Select ADCSTÑ, ADORDÑ, ADTRTY, Valor, ADTNDÑ, +                             
                        DIGITS(ADTRYY))||(DIGITS(ADTRMM))||(DIGITS(ADTRDD)) +                       
                        AS FECHA, ADTEND, ADUSER From (' +                                          
                        %Trim(SqlStr1) + ' as A '  +                                                
                       'Where a.Valor <> 0';                                                        
                                                                                                    
             Exec Sql                                                                               
               Prepare Cur From :Sqlstr2;                                                           
                                                                                                    
             Exec Sql                                                                               
               Declare C5 Cursor for Cur;                                                           
                                                                                                    
             Exec Sql                                                                               
               Open C5;                                                                             
                                                                                                    
             Exec Sql                                                                               
             Fetch C5 Into :AdcstÑ, :AdordÑ, :Adtrty, :Adtram, :AdtndÑ,                             
                          :Fecha, :Adtend, :Aduser;                                                 
          Endsr;                                                                                    
                                                                                                    
      /End-Free                                                                                     
                                                                                                    
     **********************************************************************                         
     ** Rutina Inicial del Programa                                       *                         
     **********************************************************************                         
     C     *Inzsr        Begsr                                                                      
     C     *Entry        Plist                                                                      
     C                   Parm                    Compania          3                                
                                                                                                    
     C                   Endsr                                                                      
