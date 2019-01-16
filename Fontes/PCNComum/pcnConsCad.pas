////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org                                       //
//       email: projetocooperar@zipmail.com.br                                //
//       forum: http://br.groups.yahoo.com/group/projeto_cooperar_nfe/        //
//     projeto: http://code.google.com/p/projetocooperar/                     //
//         svn: http://projetocooperar.googlecode.com/svn/trunk/              //
//                                                                            //
// Coordenação: (c) 2009 - Paulo Casagrande                                   //
//                                                                            //
//      Equipe: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//      Versão: Vide o arquivo leiame.txt na pasta raiz do projeto            //
//                                                                            //
//     Licença: GNU Lesser General Public License (GNU LGPL)                  //
//                                                                            //
//              - Este programa é software livre; você pode redistribuí-lo    //
//              e/ou modificá-lo sob os termos da Licença Pública Geral GNU,  //
//              conforme publicada pela Free Software Foundation; tanto a     //
//              versão 2 da Licença como (a seu critério) qualquer versão     //
//              mais nova.                                                    //
//                                                                            //
//              - Este programa é distribuído na expectativa de ser útil,     //
//              mas SEM QUALQUER GARANTIA; sem mesmo a garantia implícita de  //
//              COMERCIALIZAÇÃO ou de ADEQUAÇÃO A QUALQUER PROPÓSITO EM       //
//              PARTICULAR. Consulte a Licença Pública Geral GNU para obter   //
//              mais detalhes. Você deve ter recebido uma cópia da Licença    //
//              Pública Geral GNU junto com este programa; se não, escreva    //
//              para a Free Software Foundation, Inc., 59 Temple Place,       //
//              Suite 330, Boston, MA - 02111-1307, USA ou consulte a         //
//              licença oficial em http://www.gnu.org/licenses/gpl.txt        //
//                                                                            //
//    Nota (1): - Esta  licença  não  concede  o  direito  de  uso  do nome   //
//              "PCN  -  Projeto  Cooperar  NFe", não  podendo o mesmo ser    //
//              utilizado sem previa autorização.                             //
//                                                                            //
//    Nota (2): - O uso integral (ou parcial) das units do projeto esta       //
//              condicionado a manutenção deste cabeçalho junto ao código     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

{*******************************************************************************
|* Historico
|*
|* 28/09/2012: Italo
|*  - Revisado geração do XML e adicionado propriedade para controle de Versão
|*    do WebService Utilizado
*******************************************************************************}

{$I ACBr.inc}

unit pcnConsCad;

interface

uses
  SysUtils, Classes, pcnConversao, pcnGerador, pcnConsts;

type

  TConsCad = class(TPersistent)
  private
    FGerador: TGerador;
    FUF: String;
    FIE: String;
    FCNPJ: String;
    FCPF: String;
    FVersao: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
  published
    property Gerador: TGerador read FGerador write FGerador;
    property UF: String        read FUF      write FUF;
    property IE: String        read FIE      write FIE;
    property CNPJ: String      read FCNPJ    write FCNPJ;
    property CPF: String       read FCPF     write FCPF;
    property Versao: String    read FVersao  write FVersao;
  end;

implementation

{ TConsCad }

constructor TConsCad.Create;
begin
  inherited Create;

  FGerador := TGerador.Create;
end;

destructor TConsCad.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TConsCad.GerarXML: Boolean;
var
  i: integer;
begin
  Gerador.ArquivoFormatoXML := '';

  Gerador.wGrupo('ConsCad ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wGrupo('infCons');
  Gerador.wCampo(tcStr, 'GP04', 'xServ ', 008, 008, 1, 'CONS-CAD', DSC_XSERV);
  Gerador.wCampo(tcStr, 'GP05', 'UF    ', 002, 002, 1, FUF, DSC_UF);
  i := 0;
  if FIE <> EmptyStr then
   begin
     i := 1;
     Gerador.wCampo(tcStr, 'GP06', 'IE  ', 002, 014, 1, FIE, DSC_IE);
   end;
  if (FCNPJ <> EmptyStr) and (i = 0) then
   begin
     i := 1;
     Gerador.wCampoCNPJCPF('GP07', 'CNPJ', FCNPJ);
   end;
  if (FCPF <> EmptyStr) and (i = 0) then
    Gerador.wCampoCNPJCPF('GP08', 'CPF ', FCPF);
  Gerador.wGrupo('/infCons');
  Gerador.wGrupo('/ConsCad');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

