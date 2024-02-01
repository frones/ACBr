{******************************************************************************}
{ Projeto: Componentes ACBr                                                    }
{  Biblioteca multiplataforma de componentes Delphi para interação com equipa- }
{ mentos de Automação Comercial utilizados no Brasil                           }
{                                                                              }
{ Direitos Autorais Reservados (c) 2024 Daniel Simoes de Almeida               }
{                                                                              }
{ Colaboradores nesse arquivo: Italo Giurizzato Junior                         }
{                                                                              }
{  Você pode obter a última versão desse arquivo na pagina do  Projeto ACBr    }
{ Componentes localizado em      http://www.sourceforge.net/projects/acbr      }
{                                                                              }
{  Esta biblioteca é software livre; você pode redistribuí-la e/ou modificá-la }
{ sob os termos da Licença Pública Geral Menor do GNU conforme publicada pela  }
{ Free Software Foundation; tanto a versão 2.1 da Licença, ou (a seu critério) }
{ qualquer versão posterior.                                                   }
{                                                                              }
{  Esta biblioteca é distribuída na expectativa de que seja útil, porém, SEM   }
{ NENHUMA GARANTIA; nem mesmo a garantia implícita de COMERCIABILIDADE OU      }
{ ADEQUAÇÃO A UMA FINALIDADE ESPECÍFICA. Consulte a Licença Pública Geral Menor}
{ do GNU para mais detalhes. (Arquivo LICENÇA.TXT ou LICENSE.TXT)              }
{                                                                              }
{  Você deve ter recebido uma cópia da Licença Pública Geral Menor do GNU junto}
{ com esta biblioteca; se não, escreva para a Free Software Foundation, Inc.,  }
{ no endereço 59 Temple Street, Suite 330, Boston, MA 02111-1307 USA.          }
{ Você também pode obter uma copia da licença em:                              }
{ http://www.opensource.org/licenses/lgpl-license.php                          }
{                                                                              }
{ Daniel Simões de Almeida - daniel@projetoacbr.com.br - www.projetoacbr.com.br}
{       Rua Coronel Aureliano de Camargo, 963 - Tatuí - SP - 18270-170         }
{******************************************************************************}

{$I ACBr.inc}

unit ACBrDFeComum.ConsCad;

interface

uses
  SysUtils, Classes;

type

  TConsCad = class(TPersistent)
  private
    FUF: string;
    FIE: string;
    FCNPJ: string;
    FCPF: string;
    FVersao: string;
  public
    function GerarXML: string;

  published
    property UF: string     read FUF     write FUF;
    property IE: string     read FIE     write FIE;
    property CNPJ: string   read FCNPJ   write FCNPJ;
    property CPF: string    read FCPF    write FCPF;
    property Versao: string read FVersao write FVersao;
  end;

const
  NAME_SPACE = 'xmlns="http://www.portalfiscal.inf.br/nfe"';

implementation

{ TConsCad }

function TConsCad.GerarXML: string;
var
  xDoc: string;
begin
  if FIE <> EmptyStr then
    xDoc := '<IE>' + FIE + '</IE>'
  else
  begin
    if FCNPJ <> EmptyStr then
      xDoc := '<CNPJ>' + FCNPJ + '</CNPJ>'
    else
      xDoc := '<CPF>' + FCPF + '</CPF>';
  end;

  Result := '<ConsCad ' + NAME_SPACE + ' versao="' + Versao + '">' +
              '<infCons>' +
                '<xServ>CONS-CAD</xServ>' +
                '<UF>' + FUF + '</UF>' +
                xDoc +
              '</infCons>' +
            '</ConsCad>';
end;

end.

