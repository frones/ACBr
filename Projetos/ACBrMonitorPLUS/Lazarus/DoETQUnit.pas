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

unit DoETQUnit ;

interface
Uses Classes, TypInfo, SysUtils, CmdUnit, math,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;


Procedure DoETQ( Cmd : TACBrCmd ) ;

implementation
uses ACBrDevice, ACBrETQ, ACBrUtil;

function StrToChr(AStr: string; Pos: Integer): Char;
begin
  AStr   := PadLeft(AStr,1) ;
  Result := AStr[Pos];
end;

Procedure DoETQ( Cmd : TACBrCmd ) ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrETQ1 {$ELSE}dm.ACBrETQ1 {$ENDIF} do
  begin
     {$IFNDEF NOGUI}FrmACBrMonitor.{$ELSE}dm.{$ENDIF}DISWorking := True ;
     try

        if Cmd.Metodo = 'ativar' then  { Ativa o componente ETQ }
           Ativar

        else if Cmd.Metodo = 'desativar' then
           Desativar

        else if Cmd.Metodo = 'iniciaretiqueta' then
           IniciarEtiqueta

        else if Cmd.Metodo = 'finalizaretiqueta' then
           FinalizarEtiqueta(StrToIntDef(Cmd.Params(0),1),
                             StrToIntDef(Cmd.Params(1),0) )

        else if Cmd.Metodo = 'modelostr' then
           Cmd.Resposta := ModeloStr

        else if Cmd.Metodo = 'modelo' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrETQModelo),Integer(Modelo))

        else if Cmd.Metodo = 'porta' then
           Cmd.Resposta := Porta

        else if Cmd.Metodo = 'setporta' then
           Porta := Cmd.Params(0)

        else if Cmd.Metodo = 'temperatura' then
           Cmd.Resposta := IntToStr( Temperatura )

        else if Cmd.Metodo = 'settemperatura' then
           Temperatura := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'avanco' then
           Cmd.Resposta := IntToStr( Avanco )

        else if Cmd.Metodo = 'setavanco' then
           Avanco := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'unidade' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrETQUnidade),Integer(Unidade))

        else if Cmd.Metodo = 'setunidade' then
           Unidade := TACBrETQUnidade( StrToInt( Cmd.Params(0)))

        else if Cmd.Metodo = 'dpi' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrETQDPI),Integer(DPI))

        else if Cmd.Metodo = 'setdpi' then
           DPI :=  TACBrETQDPI(StrToInt( Cmd.Params(0)))

        else if Cmd.Metodo = 'origem' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrETQOrigem),Integer(Origem))

        else if Cmd.Metodo = 'setorigem' then
           Origem :=  TACBrETQOrigem(StrToInt( Cmd.Params(0)))

        else if Cmd.Metodo = 'backfeed' then
           Cmd.Resposta := GetEnumName(TypeInfo(TACBrETQBackFeed),Integer(BackFeed))

        else if Cmd.Metodo = 'setbackfeed' then
           BackFeed :=  TACBrETQBackFeed(StrToInt( Cmd.Params(0)))

        else if Cmd.Metodo = 'velocidade' then
           Cmd.Resposta := IntToStr( Velocidade )

        else if Cmd.Metodo = 'setvelocidade' then
           Velocidade := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'margemesquerda' then
           Cmd.Resposta := IntToStr( MargemEsquerda )

        else if Cmd.Metodo = 'setmargemesquerda' then
           MargemEsquerda := StrToInt( Cmd.Params(0) )

        else if Cmd.Metodo = 'imprimirtexto' then
        begin
           ImprimirTexto(
                         TACBrETQOrientacao(StrToInt(Cmd.Params(0))),  { Orientacao }
                         StrToInt(Trim(Cmd.Params(1))),                { Fonte }
                         StrToInt(Trim(Cmd.Params(2))),                { MultiplicadorH }
                         StrToInt(Trim(Cmd.Params(3))),                { MultiplicadorV }
                         StrToInt(Trim(Cmd.Params(4))),                { Vertical }
                         StrToInt(Trim(Cmd.Params(5))),                { Horizontal }
                         Cmd.Params(6),                                { Texto }
                         StrToIntDef(Cmd.Params(7), 0),                { Subfonte }
                         StrToBoolDef(Cmd.Params(8), False)            { ImprimirReverso }
                         );
        end

       else if Cmd.Metodo = 'imprimirbarras' then
           ImprimirBarras(
                         TACBrETQOrientacao(StrToInt(Cmd.Params(0))), { Orientacao }
                         TACBrTipoCodBarra(StrToInt(Cmd.Params(1))),  { TipoBarras }
                         StrToInt(Cmd.Params(2)),                     { LarguraBarraLarga }
                         StrToInt(Cmd.Params(3)),                     { LarguraBarraFina }
                         StrToInt(Cmd.Params(4)),                     { Vertical }
                         StrToInt(Cmd.Params(5)),                     { Horizontal }
                         Cmd.Params(6),                               { Texto }
                         StrToInt(Cmd.Params(7)),                     { AlturaCodBarras }
                         TACBrETQBarraExibeCodigo(StrToInt(Cmd.Params(8)))) { Exibe Codigo Barras }

        else if Cmd.Metodo = 'imprimirlinha' then
           ImprimirLinha(StrToInt(Cmd.Params(0)),   {Vertical}
                         StrToInt(Cmd.Params(1)),   {Horizontal}
                         StrToInt(Cmd.Params(2)),   {Largura}
                         StrToInt(Cmd.Params(3)))   {Altura}

        else if Cmd.Metodo = 'imprimircaixa' then
           ImprimirCaixa(StrToInt(Cmd.Params(0)),   {Vertical}
                         StrToInt(Cmd.Params(1)),   {Horizontal}
                         StrToInt(Cmd.Params(2)),   {Largura}
                         StrToInt(Cmd.Params(3)),   {Altura}
                         StrToInt(Cmd.Params(4)),   {EspessuraVertical}
                         StrToInt(Cmd.Params(5)))   {EspessuraHorizontal}

        else if Cmd.Metodo = 'imprimir' then
           Imprimir( IfThen( NaoEstaVazio( Cmd.Params(0) ), StrToInt(Cmd.Params(0)) ,
                             StrToIntDef(FrmACBrMonitor.eCopias.Text,1) ),   {Copias}
                     IfThen( NaoEstaVazio( Cmd.Params(1) ), StrToInt(Cmd.Params(1)) ,
                             StrToIntDef(FrmACBrMonitor.eAvanco.Text,0) ) )  {AvancoEtq}

        else if Cmd.Metodo = 'setlimparmemoria' then
           LimparMemoria := StrToBool( Trim(Cmd.Params(0)))

        else if Cmd.Metodo = 'limparmemoria' then
           Cmd.Resposta := BoolToStr( LimparMemoria, true )

        else if Cmd.Metodo = 'carregarimagem'  then
        begin
           CarregarImagem(Cmd.Params(0),
                          Cmd.Params(1),
                          StrToBoolDef(Trim(Cmd.Params(2)),true));
        end

        else if Cmd.Metodo = 'imprimirimagem'  then
        begin
           ImprimirImagem(StrToInt(Cmd.Params(0)),
                          StrToInt(Cmd.Params(1)),
                          StrToInt(Cmd.Params(2)),
                          Cmd.Params(3));
        end

        ELSE
           raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;

     finally
     {$IFNDEF NOGUI}FrmACBrMonitor.{$ELSE}dm.{$ENDIF}DISWorking := False ;
     end ;
  end ;
end ;

end.

