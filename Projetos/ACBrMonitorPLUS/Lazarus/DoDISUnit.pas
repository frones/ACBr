{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo:                                                 }
{                                                                              }
{  Você pode obter a última versão desse arquivo na página do Projeto ACBr     }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Este programa é software livre; você pode redistribuí-lo e/ou modificá-lo   }
{ sob os termos da Licença Pública Geral GNU, conforme publicada pela Free     }
{ Software Foundation; tanto a versão 2 da Licença como (a seu critério)       }
{ qualquer versão mais nova.                                                   }
{                                                                              }
{  Este programa é distribuído na expectativa de ser útil, mas SEM NENHUMA     }
{ GARANTIA; nem mesmo a garantia implícita de COMERCIALIZAÇÃO OU DE ADEQUAÇÃO A}
{ QUALQUER PROPÓSITO EM PARTICULAR. Consulte a Licença Pública Geral GNU para  }
{ obter mais detalhes. (Arquivo LICENCA.TXT ou LICENSE.TXT)                    }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral GNU junto com este}
{ programa; se não, escreva para a Free Software Foundation, Inc., 59 Temple   }
{ Place, Suite 330, Boston, MA 02111-1307, USA. Você também pode obter uma     }
{ copia da licença em:  http://www.opensource.org/licenses/gpl-license.php     }
{                                                                              }
{ Daniel Simões de Almeida  -  daniel@djsystem.com.br  -  www.djsystem.com.br  }
{       Rua Coronel Aureliano de Camargo, 973 - Tatuí - SP - 18270-170         }
{                                                                              }
{******************************************************************************}

{$mode objfpc}{$H+}

unit DoDISUnit ;

interface
Uses Classes, TypInfo, SysUtils, CmdUnit,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;


Procedure DoDIS( Cmd : TACBrCmd ) ;

implementation
uses ACBrDIS, ACBrUtil;

Procedure DoDIS( Cmd : TACBrCmd ) ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrDIS1 {$ELSE}dm.ACBrDIS1 {$ENDIF} do
  begin
     {$IFNDEF NOGUI}FrmACBrMonitor.{$ELSE}dm.{$ENDIF}DISWorking := True ;
     try
        if Cmd.Metodo = 'ativar' then  { Ativa o Display }
           Ativar

        else if Cmd.Metodo = 'desativar' then
           Desativar 

        else if Cmd.Metodo = 'ativo' then
           Cmd.Resposta := BoolToStr(Ativo, true)

        else if Cmd.Metodo = 'modelostr' then
           Cmd.Resposta := ModeloStr

        else if Cmd.Metodo = 'modelo' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrDISModelo),Integer(Modelo))

        else if Cmd.Metodo = 'porta' then
           Cmd.Resposta := Porta

        else if Cmd.Metodo = 'trabalhando' then
           Cmd.Resposta := BoolToStr( Trabalhando, true )

        else if Cmd.Metodo = 'linhascount' then
           Cmd.Resposta := IntToStr( LinhasCount )

        else if Cmd.Metodo = 'setlinhascount' then
           LinhasCount := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'colunas' then
           Cmd.Resposta := IntToStr( Colunas )

        else if Cmd.Metodo = 'setcolunas' then
           Colunas := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'alinhamento' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrDISAlinhamento),Integer(Alinhamento))

        else if Cmd.Metodo = 'setalinhamento' then
           Alinhamento := TACBrDISAlinhamento( GetEnumValue(
                                  TypeInfo(TACBrDISAlinhamento),Cmd.Params(0)))

        else if Cmd.Metodo = 'intervalo' then
           Cmd.Resposta := IntToStr( Intervalo )

        else if Cmd.Metodo = 'setintervalo' then
           Intervalo := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'passos' then
           Cmd.Resposta := IntToStr( Passos )

        else if Cmd.Metodo = 'setpassos' then
           Passos := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'limpardisplay' then
           LimparDisplay

        else if Cmd.Metodo = 'escrever' then
           Escrever( Cmd.Params(0) )                                   { AText }

        else if Cmd.Metodo = 'posicionarcursor' then
           PosicionarCursor( StrToInt( Cmd.Params(0) ),                { Linha }
                             StrToInt( Cmd.Params(1) ) )              { Coluna }

        else if Cmd.Metodo = 'parar' then
           Parar

        else if Cmd.Metodo = 'continuar' then
           Continuar

        else if Cmd.Metodo = 'pararlinha' then
           PararLinha( StrToInt(Cmd.Params(0)) )

        else if Cmd.Metodo = 'continuarlinha' then
           ContinuarLinha( StrToInt(Cmd.Params(0)) )

        else if Cmd.Metodo = 'exibirlinha' then
         begin
           if Cmd.Params(2) <> '' then                     // Tem 3 Parametros ?
            begin
              if LowerCase(copy(Cmd.Params(2),1,3)) = 'efe' then     // Efeito ?
                 ExibirLinha( StrToInt(Cmd.Params(0)) ,                { Linha }
                              Cmd.Params(1),                           { AText }
                   TACBrDISEfeitoExibir( GetEnumValue(
                                TypeInfo(TACBrDISEfeitoExibir),Cmd.Params(2))))
              else                                                // Alinhamento
                 ExibirLinha( StrToInt(Cmd.Params(0)) ,                { Linha }
                              Cmd.Params(1),                           { AText }
                   TACBrDISAlinhamento( GetEnumValue(
                                  TypeInfo(TACBrDISAlinhamento),Cmd.Params(2))))
            end
           else
              ExibirLinha( StrToInt(Cmd.Params(0)) ,                   { Linha }
                           Cmd.Params(1))                              { AText }
         end

        else if Cmd.Metodo = 'rolarlinha' then
           RolarLinha( StrToInt(Cmd.Params(0)) ,                       { Linha }
              TACBrDISEfeitoRolar( GetEnumValue(TypeInfo(TACBrDISEfeitoRolar),
                                   Cmd.Params(1) )) )
        ELSE
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

     finally
     {$IFNDEF NOGUI}FrmACBrMonitor.{$ELSE}dm.{$ENDIF}DISWorking := False ;
     end ;
  end ;
end ;

end.

