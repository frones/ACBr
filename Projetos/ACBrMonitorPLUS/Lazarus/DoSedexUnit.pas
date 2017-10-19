{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Rodrigues Prado Tamizou                 }
{                              Jean Patrick F. dos Santos (envio de e-mails)   }
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
unit DoSedexUnit;

interface

uses
  Classes, SysUtils, CmdUnit, IniFiles;

procedure DoSedex(Cmd: TACBrCmd);
procedure LerIniParametrosSedex(aStr: AnsiString);
Function ProcessarRespostaSedex : String;
Function ProcessarRespostaRastreio : String;

implementation
uses ACBrUtil, ACBrSedex, DoACBrUnit,  typinfo,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

Procedure DoSedex( Cmd : TACBrCmd ) ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrSedex1 {$ELSE}dm.ACBrSedex1 {$ENDIF} do
  begin
     if Cmd.Metodo = 'consultar' then
     begin
        if Cmd.Params(0) <> '' then
           LerIniParametrosSedex(Cmd.Params(0));

        Consultar;
        Cmd.Resposta := ProcessarRespostaSedex;
     end

     else if Cmd.Metodo = 'rastrear' then
     begin
        Rastrear(cmd.Params(0));
        Cmd.Resposta := ProcessarRespostaRastreio;
     end

     else
        raise Exception.Create('Comando inválido ('+Cmd.Metodo+')') ;
  end ;
end ;

procedure LerIniParametrosSedex( aStr: AnsiString ) ;
var
  MemFormatada : String;
  IniSedex: TMemIniFile;
  SL: TStringList;
  NomeSessao: String;
begin
  IniSedex := LerConverterIni(aStr);
  try
     with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrSedex1 {$ELSE}dm.ACBrSedex1 {$ENDIF} do
     begin
        NomeSessao := 'SEDEX';
        MemFormatada     := IniSedex.ReadString(NomeSessao,'Mensagem','') ;
        MemFormatada     := StringReplace( MemFormatada,'|',sLineBreak, [rfReplaceAll] );

        CepOrigem        := OnlyNumber(IniSedex.ReadString(NomeSessao,'CepOrigem',''));
        CepDestino       := OnlyNumber(IniSedex.ReadString(NomeSessao,'CepDestino',''));
        Servico          := TACBrTpServico(IniSedex.ReadInteger(NomeSessao,'Servico',0));
        Peso             := IniSedex.ReadFloat(NomeSessao,'Peso',0);
        Altura           := IniSedex.ReadFloat(NomeSessao,'Altura',0);
        Largura          := IniSedex.ReadFloat(NomeSessao,'Largura',0);
        Comprimento      := IniSedex.ReadFloat(NomeSessao,'Comprimento',0);
        Diametro         := IniSedex.ReadFloat(NomeSessao,'Diametro',0);
        ValorDeclarado   := IniSedex.ReadFloat(NomeSessao,'ValorDeclarado',0);
        Formato          := TACBrTpFormato(IniSedex.ReadInteger(NomeSessao,'Formato',0));
        AvisoRecebimento := IniSedex.ReadBool(NomeSessao,'AvisoRecebimento',False);
        MaoPropria       := IniSedex.ReadBool(NomeSessao,'MaoPropria',False);
     end;
  finally
     SL.Free;
     IniSedex.Free;
  end;
end;

function ProcessarRespostaSedex : String ;
var
  Ini : TMemIniFile ;
  Secao : String ;
  SL : TStringList ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrSedex1 {$ELSE}dm.ACBrSedex1 {$ENDIF} do
  begin
     Ini := TMemIniFile.Create('');
     SL  := TStringList.Create;
     try
        Secao := 'Consulta';
        Ini.WriteString(Secao,'CodigoServico',retCodigoServico);
        Ini.WriteFloat(Secao,'Valor',retValor);
        Ini.WriteInteger(Secao,'PrazoEntrega',retPrazoEntrega);
        Ini.WriteFloat(Secao,'ValorSemAdicionais',retValorSemAdicionais);
        Ini.WriteFloat(Secao,'ValorMaoPropria',retValorMaoPropria);
        Ini.WriteFloat(Secao,'ValorAvisoRecebimento',retValorAvisoRecebimento);
        Ini.WriteFloat(Secao,'ValorValorDeclarado',retValorValorDeclarado);
        Ini.WriteString(Secao,'EntregaDomiciliar',retEntregaDomiciliar);
        Ini.WriteString(Secao,'EntregaSabado',retEntregaSabado);
        Ini.WriteInteger(Secao,'Erro',retErro);
        Ini.WriteString(Secao,'MsgErro',retMsgErro);

        Ini.GetStrings(SL);

        Result := SL.Text;
     finally
        Ini.Free ;
        SL.Free;
     end;
  end ;
end;

function ProcessarRespostaRastreio: String;
var
  Ini : TMemIniFile ;
  Secao : String ;
  SL : TStringList ;
  I: Integer;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrSedex1 {$ELSE}dm.ACBrSedex1 {$ENDIF} do
  begin
     Ini := TMemIniFile.Create('');
     SL  := TStringList.Create;
     try
        For I := 0 to retRastreio.Count-1 do
        begin
          Secao := 'Rastreio'+IntToStrZero(I,2);

          Ini.WriteDateTime(Secao,'DataHora',retRastreio[I].DataHora);
          Ini.WriteString(Secao,'Local',retRastreio[I].Local);
          Ini.WriteString(Secao,'Situacao',retRastreio[I].Situacao);
          Ini.WriteString(Secao,'Observacao',retRastreio[I].Observacao);
        end;

        Ini.GetStrings(SL);

        Result := SL.Text;
     finally
        Ini.Free ;
        SL.Free;
     end;
  end ;
end;

end.

