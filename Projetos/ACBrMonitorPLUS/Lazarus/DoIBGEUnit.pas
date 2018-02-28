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

unit DoIBGEUnit;

interface

uses
  Classes, SysUtils, CmdUnit, IniFiles;

procedure DoIBGE(Cmd: TACBrCmd);
Function CidadesToINI : String;

implementation

uses ACBrUtil,
  {$IFNDEF NOGUI}ACBrMonitor1 {$ELSE}ACBrMonitorConsoleDM {$ENDIF} ;

procedure DoIBGE( Cmd: TACBrCmd ) ;
begin
   with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrIBGE1 {$ELSE}dm.ACBrIBGE1 {$ENDIF} do
   begin
      if Cmd.Metodo = 'buscarporcodigo' then
       begin
         BuscarPorCodigo( StrToInt( Cmd.Params(0) ) );              { Cod.IBGE }
         Cmd.Resposta := CidadesToINI;
       end

      else if Cmd.Metodo = 'buscarpornome' then
       begin
         BuscarPorNome( Cmd.Params(0) ) ;                             { Cidade }
         Cmd.Resposta := CidadesToINI;
       end

      ELSE
         raise Exception.Create('Comando inválido ('+Cmd.Comando+')') ;
   end;

end;

function CidadesToINI : String ;
var
  I : Integer ;
  Ini : TMemIniFile ;
  Secao : String ;
  SL : TStringList ;
begin
  with {$IFNDEF NOGUI}FrmACBrMonitor.ACBrIBGE1 {$ELSE}dm.ACBrIBGE1 {$ENDIF} do
  begin
     if Cidades.Count < 1 then
        raise Exception.Create( 'Nenhuma Cidade encontrada' ) ;

     Ini := TMemIniFile.Create('');
     SL  := TStringList.Create;
     try
       For I := 0 to Cidades.Count-1 do
       begin
          Secao := 'Cidade'+IntToStr(I+1);

          Ini.WriteString(Secao,'UF',         Cidades[I].UF);
          Ini.WriteString(Secao,'CodUF',      IntToStr(Cidades[I].CodUF) );
          Ini.WriteString(Secao,'Municipio',  Cidades[I].Municipio);
          Ini.WriteString(Secao,'CodMunicio', IntToStr(Cidades[I].CodMunicipio) );
          Ini.WriteString(Secao,'Area',       FloatToStr( Cidades[I].Area) );
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

