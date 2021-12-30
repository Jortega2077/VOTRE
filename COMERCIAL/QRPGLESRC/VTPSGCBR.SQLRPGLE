      ***********************************************************************                       
      ** Programa      : VTPSGCAR                                          **                       
      ** Descripciòn   : PGM que Verifica Diferencias en el VTMSPGF        **                       
      ** Proyecto      : Plan de Guías                                     **                       
      ** Autor         : (JOrtega)                                         **                       
      ** Fecha Creación: Febrero 24 de 2021                                **                       
      ***********************************************************************                       
     ** Definición de Directivas de Compilación                                                     
     HOption(*NoDebugIO) Dftactgrp(*No)                                                             
                                                                                                    
     FVtwlpgf   If a E           k disk                                                             
                                                                                                    
      **Definición de Variables de Trabajo                                                          
     DRegion           s             10a   inz(' ')                                                 
     DNombre           s             30a   inz(' ')                                                 
     DSpgvdsc          s             11  2 inz(0)                                                   
     DSpgccrc          s              3  0 inz(0)                                                   
     DSpginpc          s              1a   inz(' ')                                                 
     DSpgfbtc          s             10a   inz(' ')                                                 
     DSpgvdse          s             11  2 inz(0)                                                   
     DSpgccre          s              3  0 inz(0)                                                   
     DSpginpe          s              1a   inz(' ')                                                 
     DSpgfbte          s             10a   inz(' ')                                                 
     DSpgcia           s              3a                                                            
     DSpgcam           s              4a                                                            
     DSpgzon           s              3a                                                            
     DSpgsec           s              1a                                                            
     DSpgced           s              9  0                                                          
                                                                                                    
     DMain             Pr                  Extpgm('Main')                                           
     DWcia                            3a                                                            
     DWcmp                            4a                                                            
     ** Parametro de Entrada al Programa (Entry)                                                    
     DMain             Pi                                                                           
     DWcia                            3a                                                            
     DWcmp                            4a                                                            
                                                                                                    
      //////////////////////////////////////////////////////////////////////                        
      //            P R O G R A M A    P R I N C I P A L                  //                        
      //////////////////////////////////////////////////////////////////////                        
      /Free                                                                                         
       Exec Sql                                                                                     
        Declare C1 Cursor For                                                                       
        Select Spgcia, Spgcam, Spgzon, Spgsec, Spgced                                               
        From Votrea00/Vtmspgf A Left Join Votrea00/Vtwlpgf B                                        
        On A.Spgcia = B.Lpgcia And                                                                  
           A.Spgcam = B.Lpgcmp And                                                                  
           A.Spgzon = B.Lpgzna And                                                                  
           A.Spgsec = B.Lpgsec And                                                                  
           A.Spgced = B.Lpgcdg                                                                      
        Where A.Spgcia = :Wcia And                                                                  
              A.Spgcam >= :Wcmp And B.Lpgcdg Is Null;                                               
                                                                                                    
       Exec Sql                                                                                     
        Open C1;                                                                                    
                                                                                                    
       Exec Sql                                                                                     
        Fetch C1 Into :Spgcia, :Spgcam, :Spgzon, :Spgsec, :Spgced;                                  
                                                                                                    
       Dow SqlCod <> 100 And SqlCod >= *Zeros;                                                      
         Clear Region;                                                                              
         Exec Sql                                                                                   
          Select Sminf2 Into :Region                                                                
          From Pslman                                                                               
          Where Smslsp =:Spgzon;                                                                    
                                                                                                    
         Clear Nombre;                                                                              
         Exec Sql                                                                                   
          Select Mlname Into :Nombre                                                                
          From Lmllst01                                                                             
          Where MlcstÑ =:Spgced;                                                                    
          Chain (Spgcia:Spgcam:Region:Spgzon:Spgsec:Spgced:Lpgdsa:                                  
                 Lpgipt) Vtwlpgf;                                                                   
          If Not %Found(Vtwlpgf);                                                                   
           Lpgcia = Spgcia;                                                                         
           Lpgcmp = Spgcam;                                                                         
           Lpgreg = Region;                                                                         
           Lpgzna = Spgzon;                                                                         
           Lpgsec = Spgsec;                                                                         
           Lpgcdg = Spgced;                                                                         
           Lpgnom = Nombre;                                                                         
           Clear Spgvdsc;                                                                           
           Clear Spgccrc;                                                                           
           Clear Spginpc;                                                                           
           Clear Spgfbtc;                                                                           
           Exec Sql                                                                                 
            Select Spgvds, Spgccr, Spginp, Spgfbt                                                   
            Into :Spgvdsc, :Spgccrc, :Spginpc, :Spgfbtc                                             
            From Votrea00/Vtmspgf                                                                   
            Where Spgcia = :Spgcia And                                                              
                  Spgzon = :Spgzon And                                                              
                  Spgcam = :Spgcam And                                                              
                  Spgsec = :Spgsec And                                                              
                  Spgced = :Spgced And                                                              
                  Spgccr = 042;                                                                     
                                                                                                    
           Clear Spgvdse;                                                                           
           Clear Spgccre;                                                                           
           Clear Spginpe;                                                                           
           Clear Spgfbte;                                                                           
           Exec Sql                                                                                 
            Select Spgvds, Spgccr, Spginp, Spgfbt                                                   
            Into :Spgvdsc, :Spgccrc, :Spginpc, :Spgfbtc                                             
            From Votrea00/Vtmspgf                                                                   
            Where Spgcia = :Spgcia And                                                              
                  Spgzon = :Spgzon And                                                              
                  Spgcam = :Spgcam And                                                              
                  Spgsec = :Spgsec And                                                              
                  Spgced = :Spgced And                                                              
                  Spgccr = 043;                                                                     
                                                                                                    
            Lpgtds = Spgvdsc + Spgvdse;                                                             
            Lpgvde = Spgvdse;                                                                       
            Lpgfde = Spgfbte;                                                                       
            Lpgvdc = Spgvdsc;                                                                       
            Lpgfdc = Spgfbtc;                                                                       
            LPGvrc = Spgvdsc;                                                                       
            Lpgfvr = Spgfbtc;                                                                       
            If Spginpc = ' ' And Spginpe = ' ';                                                     
             Lpgdsa = 'SI';                                                                         
             Lpgmot = ' ';                                                                          
            ElseIf Spginpc = 'E';                                                                   
             Lpgdsa = 'NO';                                                                         
             Lpgmot = 'NO COLOCO PEDIDO EN LA CAMPAÑA ACTUAL';                                      
            Endif;                                                                                  
            Lpgipt = ' ';                                                                           
            Write Reglpg;                                                                           
           Endif;                                                                                   
         Exec Sql                                                                                   
          Fetch C1 Into :Spgcia, :Spgcam, :Spgzon, :Spgsec, :Spgced;                                
       Enddo;                                                                                       
                                                                                                    
       Exec Sql                                                                                     
        Close C1;                                                                                   
                                                                                                    
       *InLr = *On;                                                                                 
      /End-Free                                                                                     
