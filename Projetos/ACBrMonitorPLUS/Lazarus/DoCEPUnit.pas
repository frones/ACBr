{******************************************************************************}
{ Projeto: ACBr Monitor                                                        }
{  Executavel multiplataforma que faz uso do conjunto de componentes ACBr para }
{ criar uma interface de comunicação com equipamentos de automacao comercial.  }
{                                                                              }
{ Direitos Autorais Reservados (c) 2010 Daniel Simões de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Juliana Rodrigues Prado Tamizou                 }
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

unit DoCEPUnit;

interface

uses
  Classes, SysUtils, CmdUnit, IniFiles;

procedure DoCEP(Cmd: TACBrCmd);
Function EnderecosToINI : String;

implementation

uses ACBrUtil,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

procedure DoCEP( Cmd: TACBrCmd ) ;
begin
   with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrCEP1 {$ELSE}dm.ACBrCEP1 {$ENDIF} do
   begin
      if Cmd.Metodo = 'buscarporcep' then
       begin
         BuscarPorCEP( Cmd.Params(0) );                                  { CEP }
         Cmd.Resposta := EnderecosToINI;
       end

      else if Cmd.Metodo = 'buscarporlogradouro' then
       begin
         BuscarPorLogradouro( Cmd.Params(0),                          { Cidade }
                              Cmd.Params(1),        { Tipo Logradouro Ex: "Rua"}
                              Cmd.Params(2),                      { Logradouro }
                              Cmd.Params(3),                              { UF }
                              Cmd.Params(4) ) ;                       { Bairro }
         Cmd.Resposta := EnderecosToINI;
       end

      ELSE
         raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;
   end;

end;

function EnderecosToINI : String ;
var
  I : Integer ;
  Ini : TMemIniFile ;
  Secao : String ;
  SL : TStringList ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrCEP1 {$ELSE}dm.ACBrCEP1 {$ENDIF} do
  begin
     if Enderecos.Count < 1 then
        raise Exception.Create( 'Nenhum endereço encontrado' ) ;

     Ini := TMemIniFile.Create('');
     SL  := TStringList.Create;
     try
       For I := 0 to Enderecos.Count-1 do
       begin
          Secao := 'Endereco'+IntToStr(I+1);

          Ini.WriteString(Secao,'CEP',             Enderecos[I].CEP);
          Ini.WriteString(Secao,'Tipo_Logradouro', Enderecos[I].Tipo_Logradouro);
          Ini.WriteString(Secao,'Logradouro',      Enderecos[I].Logradouro);
          Ini.WriteString(Secao,'Complemento',     Enderecos[I].Complemento);
          Ini.WriteString(Secao,'Bairro',          Enderecos[I].Bairro);
          Ini.WriteString(Secao,'Municipio',       Enderecos[I].Municipio);
          Ini.WriteString(Secao,'UF',              Enderecos[I].UF);
          Ini.WriteString(Secao,'IBGE_Municipio',  Enderecos[I].IBGE_Municipio);
          Ini.WriteString(Secao,'IBGE_UF',         Enderecos[I].IBGE_UF);
       end ;

       Ini.GetStrings(SL);

       Result := SL.Text;
     finally
        Ini.Free ;
        SL.Free;
     end;
  end ;
end;

end.

