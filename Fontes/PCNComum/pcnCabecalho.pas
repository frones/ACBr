////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//              PCN - Projeto Cooperar NFe                                    //
//                                                                            //
//   Descrição: Classes para geração/leitura dos arquivos xml da NFe          //
//                                                                            //
//        site: www.projetocooperar.org/nfe                                   //
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

{$I ACBr.inc}

unit pcnCabecalho;

interface

uses
  SysUtils, Classes, pcnConversao, pcnGerador;

type
  TCabecalho = class(TPersistent)
  private
    FGerador: TGerador;
    FVersao: String;
    FVersaoDados: String;
  public
    constructor Create;
    destructor Destroy; override;
    function GerarXML: Boolean;
  published
    property Gerador: TGerador   read FGerador     write FGerador;
    property Versao: String      read FVersao      write FVersao;
    property VersaoDados: String read FVersaoDados write FVersaoDados;
  end;

implementation

{ TCabecalho }

constructor TCabecalho.Create;
begin
  FGerador := TGerador.Create;
end;

destructor TCabecalho.Destroy;
begin
  FGerador.Free;
  inherited;
end;

function TCabecalho.GerarXML: Boolean;
begin
  Gerador.ArquivoFormatoXML := '';
  
  Gerador.wGrupo(ENCODING_UTF8_STD, '', False);
  Gerador.wGrupo('cabecMsg ' + NAME_SPACE + ' versao="' + Versao + '"');
  Gerador.wCampo(tcStr, 'FP03', 'versaoDados', 001, 001, 1, VersaoDados, DSC_VERPROC);
  Gerador.wGrupo('/cabecMsg');

  Result := (Gerador.ListaDeAlertas.Count = 0);
end;

end.

