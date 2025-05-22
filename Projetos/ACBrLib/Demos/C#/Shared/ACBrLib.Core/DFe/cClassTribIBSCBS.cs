using System.ComponentModel;

namespace ACBrLib.Core.DFe
{
    public enum cClassTribIBSCBS
    {
        [EnumValue("")]
        cctVazio,

        [EnumValue("000001")]
        [Description("Situações tributadas integralmente pelo IBS e CBS")]
        cct000001,

        [EnumValue("000002")]
        [Description("Exploração de via, observado o art. 11 da Lei Complementar nº 214, de 2025.")]
        cct000002,

        [EnumValue("000003")]
        [Description("Regime automotivo - projetos incentivados, observado o art. 311 da Lei Complementar nº 214, de 2025.")]
        cct000003,

        [EnumValue("000004")]
        [Description("Regime automotivo - projetos incentivados, observado o art. 312 da Lei Complementar nº 214, de 2025.")]
        cct000004,

        [EnumValue("010001")]
        [Description("Operações do FGTS não realizadas pela Caixa Econômica Federal, observado o art. 212 da Lei Complementar nº 214, de 2025.")]
        cct010001,

        [EnumValue("011001")]
        [Description("Planos de assistência funerária, observado o art. 236 da Lei Complementar nº 214, de 2025.")]
        cct011001,

        [EnumValue("011002")]
        [Description("Planos de assistência à saúde, observado o art. 237 da Lei Complementar nº 214, de 2025.")]
        cct011002,

        [EnumValue("011003")]
        [Description("Intermediação de planos de assistência à saúde, observado o art. 240 da Lei Complementar nº 214, de 2025.")]
        cct011003,

        [EnumValue("011004")]
        [Description("Concursos e prognósticos, observado o art. 246 da Lei Complementar nº 214, de 2025.")]
        cct011004,

        [EnumValue("011005")]
        [Description("Planos de assistência à saúde de animais domésticos, observado o art. 243 da Lei Complementar nº 214, de 2025.")]
        cct011005,

        [EnumValue("200001")]
        [Description("Aquisições de máquinas, de aparelhos, de instrumentos, de equipamentos, de matérias-primas, de produtos intermediários e de materiais de embalagem realizadas entre empresas autorizadas a operar em zonas de processamento de exportação, observado o art. 103 da Lei Complementar nº 214, de 2025.")]
        cct200001,

        [EnumValue("200002")]
        [Description("Fornecimento ou importação de tratores, máquinas e implementos agrícolas, destinados a produtor rural não contribuinte, e de veículos de transporte de carga destinados a transportador autônomo de carga pessoa física não contribuinte, observado o art. 110 da Lei Complementar nº 214, de 2025.")]
        cct200002,

        [EnumValue("200003")]
        [Description("Vendas de produtos destinados à alimentação humana relacionados no Anexo I da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, que compõem a Cesta Básica Nacional de Alimentos, criada nos termos do art. 8º da Emenda Constitucional nº 132, de 20 de dezembro de 2023, observado o art. 125 da Lei Complementar nº 214, de 2025.")]
        cct200003,

        [EnumValue("200004")]
        [Description("Venda de dispositivos médicos com a especificação das respectivas classificações da NCM/SH previstas no Anexo XII da Lei Complementar nº 214, de 2025, observado o art. 144 da Lei Complementar nº 214, de 2025.")]
        cct200004,

        [EnumValue("200005")]
        [Description("Venda de dispositivos médicos com a especificação das respectivas classificações da NCM/SH previstas no Anexo IV da Lei Complementar nº 214, de 2025, quando adquiridos por órgãos da administração pública direta, autarquias e fundações públicas, observado o art. 144 da Lei Complementar nº 214, de 2025.")]
        cct200005,

        [EnumValue("200006")]
        [Description("Situação de emergência de saúde pública reconhecida pelo Poder Legislativo federal, estadual, distrital ou municipal competente, ato conjunto do Ministro da Fazenda e do Comitê Gestor do IBS poderá ser editado, a qualquer momento, para incluir dispositivos não listados no Anexo XIII da Lei Complementar nº 214, de 2025, limitada a vigência do benefício ao período e à localidade da emergência de saúde pública, observado o art. 144 da Lei Complementar nº 214, de 2025.")]
        cct200006,

        [EnumValue("200007")]
        [Description("Fornecimento dos dispositivos de acessibilidade próprios para pessoas com deficiência relacionados no Anexo XIV da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 145 da Lei Complementar nº 214, de 2025.")]
        cct200007,

        [EnumValue("200008")]
        [Description("Fornecimento dos dispositivos de acessibilidade próprios para pessoas com deficiência relacionados no Anexo V da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, quando adquiridos por órgãos da administração pública direta, autarquias, fundações públicas e entidades imunes, observado o art. 145 da Lei Complementar nº 214, de 2025.")]
        cct200008,

        [EnumValue("200009")]
        [Description("Fornecimento dos medicamentos relacionados no Anexo XIV da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 146 da Lei Complementar nº 214, de 2025.")]
        cct200009,

        [EnumValue("200010")]
        [Description("Fornecimento dos medicamentos registrados na Anvisa, quando adquiridos por órgãos da administração pública direta, autarquias, fundações públicas e entidades imunes, observado o art. 146 da Lei Complementar nº 214, de 2025.")]
        cct200010,

        [EnumValue("200011")]
        [Description("Fornecimento das composições para nutrição enteral e parenteral, composições especiais e fórmulas nutricionais destinadas às pessoas com erros inatos do metabolismo relacionadas no Anexo VI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, quando adquiridas por órgãos da administração pública direta, autarquias e fundações públicas, observado o art. 146 da Lei Complementar nº 214, de 2025.")]
        cct200011,

        [EnumValue("200013")]
        [Description("Fornecimento de tampões higiênicos, absorventes higiênicos internos ou externos, descartáveis ou reutilizáveis, calcinhas absorventes e coletores menstruais, observado o art. 147 da Lei Complementar nº 214, de 2025.")]
        cct200013,

        [EnumValue("200014")]
        [Description("Fornecimento dos produtos hortícolas, frutas e ovos, relacionados no Anexo XV da Lei Complementar nº 214 , de 2025, com a especificação das respectivas classificações da NCM/SH e desde que não cozidos, observado o art. 148 da Lei Complementar nº 214, de 2025.")]
        cct200014,

        [EnumValue("200015")]
        [Description("Venda de automóveis de passageiros de fabricação nacional de, no mínimo, 4 (quatro) portas, inclusive a de acesso ao bagageiro, quando adquiridos por motoristas profissionais que exerçam, comprovadamente, em automóvel de sua propriedade, atividade de condutor autônomo de passageiros, na condição de titular de autorização, permissão ou concessão do poder público, e que destinem o automóvel à utilização na categoria de aluguel (táxi), ou por pessoas com deficiência física, visual, auditiva, deficiência mental severa ou profunda, transtorno do espectro autista, com prejuízos na \r\n comunicação social e em padrões restritos ou repetitivos de comportamento de nível moderado ou grave, nos termos da legislação relativa à matéria, observado o disposto no art. 149 da Lei Complementar nº 214, de 2025.")]
        cct200015,

        [EnumValue("200016")]
        [Description("Prestação de serviços de pesquisa e desenvolvimento por Instituição Científica, Tecnológica e de Inovação (ICT) sem fins lucrativos para a administração pública direta, autarquias e fundações públicas ou para o contribuinte sujeito ao regime regular do IBS e da CBS, observado o disposto no art. 156  da Lei Complementar nº 214, de 2025.")]
        cct200016,

        [EnumValue("200017")]
        [Description("Operações relacionadas ao FGTS, considerando aquelas necessárias à aplicação da Lei nº 8.036, de 1990, realizadas pelo Conselho Curador ou Secretaria Executiva do FGTS, observado o art. 212 da Lei Complementar nº 214, de 2025.")]
        cct200017,

        [EnumValue("200018")]
        [Description("Operações de resseguro e retrocessão ficam sujeitas à incidência à alíquota zero, inclusive quando os prêmios de resseguro e retrocessão forem cedidos ao exterior, observado o art. 223 da Lei Complementar nº 214, de 2025.")]
        cct200018,

        [EnumValue("200019")]
        [Description("Importador dos serviços financeiros seja contribuinte que realize as operações de que tratam os incisos I a V do caput do art. 182, será aplicada alíquota zero na importação, sem prejuízo da manutenção do direito de dedução dessas despesas da base de cálculo do IBS e da CBS, segundo, observado o art. 231 da Lei Complementar nº 214, de 2025.")]
        cct200019,

        [EnumValue("200020")]
        [Description("Operação praticada por sociedades cooperativas optantes por regime específico do IBS e CBS, quando o associado destinar bem ou serviço à cooperativa de que participa, e a cooperativa fornecer bem ou serviço ao associado sujeito ao regime regular do IBS e da CBS, observado o art. 271 da Lei Complementar nº 214, de 2025.")]
        cct200020,

        [EnumValue("200021")]
        [Description("Serviços de transporte público coletivo de passageiros ferroviário e hidroviário urbanos, semiurbanos e metropolitanos, observado o art. 285 da Lei Complementar nº 214, de 2025.")]
        cct200021,

        [EnumValue("200022")]
        [Description("Operação originada fora da Zona Franca de Manaus que destine bem material industrializado de origem nacional a contribuinte estabelecido na Zona Franca de Manaus que seja habilitado nos termos do art. 442 da Lei Complementar nº 214, de 2025, e sujeito ao regime regular do IBS e da CBS ou optante pelo regime do Simples Nacional de que trata o art. 12 da Lei Complementar nº 123, de 2006, observado o art. 445 da Lei Complementar nº 214, de 2025.")]
        cct200022,

        [EnumValue("200023")]
        [Description("Operação realizada por indústria incentivada que destine bem material intermediário para outra indústria incentivada na Zona Franca de Manaus, desde que a entrega ou disponibilização dos bens ocorra dentro da referida área, observado o art. 448 da Lei Complementar nº 214, de 2025.")]
        cct200023,

        [EnumValue("200024")]
        [Description("Operação originada fora das Áreas de Livre Comércio que destine bem material industrializado de origem nacional a contribuinte estabelecido nas Áreas de Livre Comércio que seja habilitado nos termos do art. 456 da Lei Complementar nº 214, de 2025, e sujeito ao regime regular do IBS e da CBS ou optante pelo regime do Simples Nacional de que trata o art. 12 da Lei Complementar nº 123, de 2006, observado o art. 463 da Lei Complementar nº 214, de 2025.")]
        cct200024,

        [EnumValue("200025")]
        [Description("Fornecimento dos serviços de educação relacionados ao Programa Universidade para Todos (Prouni), instituído pela Lei nº 11.096, de 13 de janeiro de 2005, observado o art. 308 da Lei Complementar nº 214, de 2025.")]
        cct200025,

        [EnumValue("200026")]
        [Description("Locação de imóveis localizados nas zonas reabilitadas, pelo prazo de 5 (cinco) anos, contado da data de expedição do habite-se, e relacionados a projetos de reabilitação urbana de zonas históricas e de áreas críticas de recuperação e reconversão urbanística dos Municípios ou do Distrito Federal, a serem delimitadas por lei municipal ou distrital, observado o art. 158 da Lei Complementar nº 214, de 2025.")]
        cct200026,

        [EnumValue("200027")]
        [Description("Operações de locação, cessão onerosa e arrendamento de bens imóveis, observado o art. 261 da Lei Complementar nº 214, de 2025.")]
        cct200027,

        [EnumValue("200028")]
        [Description("Fornecimento dos serviços de educação relacionados no Anexo II da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da Nomenclatura Brasileira de Serviços, Intangíveis e Outras Operações que Produzam Variações no Patrimônio (NBS), observado o art. 129 da Lei Complementar nº 214, de 2025.")]
        cct200028,

        [EnumValue("200029")]
        [Description("Fornecimento dos serviços de saúde humana relacionados no Anexo III da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS, observado o art. 130 da Lei Complementar nº 214, de 2025.")]
        cct200029,

        [EnumValue("200030")]
        [Description("Venda dos dispositivos médicos relacionados no Anexo IV da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 131 da Lei Complementar nº 214, de 2025.")]
        cct200030,

        [EnumValue("200031")]
        [Description("Fornecimento dos dispositivos de acessibilidade próprios para pessoas com deficiência relacionados no Anexo V da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 132 da Lei Complementar nº 214, de 2025.")]
        cct200031,

        [EnumValue("200032")]
        [Description("Fornecimento dos medicamentos registrados na Anvisa ou produzidos por farmácias de manipulação, ressalvados os medicamentos sujeitos à alíquota zero de que trata o art. 141 da Lei Complementar nº 214, de 2025, observado o art. 133 da Lei Complementar nº 214, de 2025.")]
        cct200032,

        [EnumValue("200033")]
        [Description("Fornecimento das composições para nutrição enteral e parenteral, composições especiais e fórmulas nutricionais destinadas às pessoas com erros inatos do metabolismo relacionadas no Anexo VI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 133 da Lei Complementar nº 214, de 2025.")]
        cct200033,

        [EnumValue("200034")]
        [Description("Fornecimento dos alimentos destinados ao consumo humano relacionados no Anexo VII da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 135 da Lei Complementar nº 214, de 2025.")]
        cct200034,

        [EnumValue("200035")]
        [Description("Fornecimento dos produtos de higiene pessoal e limpeza relacionados no Anexo VIII da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH, observado o art. 136 da Lei Complementar nº 214, de 2025.")]
        cct200035,

        [EnumValue("200036")]
        [Description("Fornecimento de produtos agropecuários, aquícolas, pesqueiros, florestais e extrativistas vegetais in natura, observado o art. 137 da Lei Complementar nº 214, de 2025.")]
        cct200036,

        [EnumValue("200037")]
        [Description("Fornecimento de serviços ambientais de conservação ou recuperação da vegetação nativa, mesmo que fornecidos sob a forma de manejo sustentável de sistemas agrícolas, agroflorestais e agrossilvopastoris, em conformidade com as definições e requisitos da legislação específica, observado o art. 137 da Lei Complementar nº 214, de 2025.")]
        cct200037,

        [EnumValue("200038")]
        [Description("Fornecimento dos insumos agropecuários e aquícolas relacionados no Anexo IX da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NCM/SH e da NBS, observado o art. 138 da Lei Complementar nº 214, de 2025.")]
        cct200038,

        [EnumValue("200039")]
        [Description("Fornecimento dos serviços e o licenciamento ou cessão dos direitos relacionados no Anexo X da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS, quando destinados às seguintes produções nacionais artísticas, culturais, de eventos, jornalísticas e audiovisuais: espetáculos teatrais, circenses e de dança, shows musicais, desfiles carnavalescos ou folclóricos, eventos acadêmicos e científicos, como congressos, conferências e simpósios, feiras de negócios, exposições, feiras e mostras culturais, artísticas e literárias; programas de auditório ou jornalísticos, filmes, documentários, séries, novelas, entrevistas e clipes musicais, observado o art. 139 da Lei Complementar nº 214, de 2025.")]
        cct200039,

        [EnumValue("200040")]
        [Description("Fornecimento dos seguintes serviços de comunicação institucional à administração pública direta, autarquias e fundações públicas: serviços direcionados ao planejamento, criação, programação e manutenção de páginas eletrônicas da administração pública, ao monitoramento e gestão de suas redes sociais e à otimização de páginas e canais digitais para mecanismos de buscas e produção de mensagens, infográficos, painéis interativos e conteúdo institucional, serviços de relações com a imprensa, que reúnem estratégias organizacionais para promover e reforçar a comunicação dos órgãos e das entidades contratantes com seus públicos de interesse, por meio da interação com profissionais da imprensa, e serviços de relações públicas, que compreendem o esforço de comunicação planejado, coeso e contínuo que tem por objetivo estabelecer adequada percepção da atuação e dos objetivos institucionais, a partir do estímulo à compreensão mútua e da manutenção de padrões de relacionamento e fluxos de informação entre os órgãos e as entidades contratantes e seus públicos de interesse, no País e no exterior, observado o art. 140 da Lei Complementar nº 214, de 2025.")]
        cct200040,

        [EnumValue("200041")]
        [Description("Operações relacionadas às seguintes atividades desportivas: fornecimento de serviço de educação desportiva, classificado no código 1.2205.12.00 da NBS, e gestão e exploração do desporto por associações e clubes esportivos filiados ao órgão estadual ou federal responsável pela coordenação dos desportos, inclusive por meio de venda de ingressos para eventos desportivos, fornecimento oneroso ou não de bens e serviços, inclusive ingressos, por meio de programas de sócio-torcedor, cessão dos direitos desportivos dos atletas e transferência de atletas para outra entidade desportiva ou seu retorno à atividade em outra entidade desportiva, observado o art. 141 da Lei Complementar nº 214, de 2025.")]
        cct200041,

        [EnumValue("200042")]
        [Description("Operações relacionadas ao fornecimento de serviço de educação desportiva, classificado no código 1.2205.12.00 da NBS, observado o art. 141 da Lei Complementar nº 214, de 2025.")]
        cct200042,

        [EnumValue("200043")]
        [Description("Fornecimento à administração pública direta, autarquias e fundações púbicas dos serviços e dos bens relativos à soberania e à segurança nacional, à segurança da informação e à segurança cibernética relacionados no Anexo XI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS e da NCM/SH, observado o art. 142 da Lei Complementar nº 214, de 2025.")]
        cct200043,

        [EnumValue("200044")]
        [Description("Operações e prestações de serviços de segurança da informação e segurança cibernética desenvolvidos por sociedade que tenha sócio brasileiro com o mínimo de 20% (vinte por cento) do seu capital social, relacionados no Anexo XI da Lei Complementar nº 214, de 2025, com a especificação das respectivas classificações da NBS e da NCM/SH, observado o art. 142 da Lei Complementar nº 214, de 2025.")]
        cct200044,

        [EnumValue("200045")]
        [Description("Operações relacionadas a projetos de reabilitação urbana de zonas históricas e de áreas críticas de recuperação e reconversão urbanística dos Municípios ou do Distrito Federal, a serem delimitadas por lei municipal ou distrital, observado o art. 158 da Lei Complementar nº 214, de 2025.")]
        cct200045,

        [EnumValue("200046")]
        [Description("Operações com bens imóveis, observado o art. 261 da Lei Complementar nº 214, de 2025.")]
        cct200046,

        [EnumValue("200047")]
        [Description("Bares e Restaurantes, observado o art. 275 da Lei Complementar nº 214, de 2025.")]
        cct200047,

        [EnumValue("200048")]
        [Description("Hotelaria, Parques de Diversão e Parques Temáticos, observado o art. 281 da Lei Complementar nº 214, de 2025.")]
        cct200048,

        [EnumValue("200049")]
        [Description("Transporte coletivo de passageiros rodoviário, ferroviário e hidroviário intermunicipais e interestaduais, observado o art. 286 da Lei Complementar nº 214, de 2025.")]
        cct200049,

        [EnumValue("200450")]
        [Description("Serviços de transporte aéreo regional coletivo de passageiros ou de carga, observado o art. 287 da Lei Complementar nº 214, de 2025.")]
        cct200450,

        [EnumValue("200051")]
        [Description("Agências de Turismo, , observado o art. 289 da Lei Complementar nº 214, de 2025.")]
        cct200051,

        [EnumValue("200052")]
        [Description("Prestação de serviços das seguintes profissões intelectuais de natureza científica, literária ou artística, submetidas à fiscalização por conselho profissional: administradores, advogados, arquitetos e urbanistas, assistentes sociais, bibliotecários, biólogos, contabilistas, economistas, economistas domésticos, profissionais de educação física, engenheiros e agrônomos, estatísticos, médicos veterinários e zootecnistas, museólogos, químicos, profissionais de relações públicas, técnicos industriais e técnicos agrícolas, observado o art. 127 da Lei Complementar nº 214, de 2025.")]
        cct200052,

        [EnumValue("210001")]
        [Description("Redutor social aplicado uma única vez na alienação de bem imóvel residencial novo, observado o art. 259 da Lei Complementar nº 214, de 2025.")]
        cct210001,

        [EnumValue("210002")]
        [Description("Redutor social aplicado uma única vez na alienação de lote residencial, observado o art. 259 da Lei Complementar nº 214, de 2025.")]
        cct210002,

        [EnumValue("210003")]
        [Description("Redutor social em operações de locação, cessão onerosa e arrendamento de bens imóveis de uso residencial, observado o art. 260 da Lei Complementar nº 214, de 2025.")]
        cct210003,

        [EnumValue("220001")]
        [Description("Incorporação imobiliária submetida ao regime especial de tributação, observado o art. 485 da Lei Complementar nº 214, de 2025.")]
        cct220001,

        [EnumValue("220002")]
        [Description("Incorporação imobiliária submetida ao regime especial de tributação, observado o art. 485 da Lei Complementar nº 214, de 2025.")]
        cct220002,

        [EnumValue("220003")]
        [Description("Alienação de imóvel decorrente de parcelamento do solo, observado o art. 486 da Lei Complementar nº 214, de 2025.")]
        cct220003,

        [EnumValue("221001")]
        [Description("Locação, cessão onerosa ou arrendamento de bem imóvel com alíquota sobre a receita bruta, observado o art. 487 da Lei Complementar nº 214, de 2025.")]
        cct221001,

        [EnumValue("400001")]
        [Description("Fornecimento de serviços de transporte público coletivo de passageiros rodoviário e metroviário de caráter urbano, semiurbano e metropolitano, sob regime de autorização, permissão ou concessão pública, observado o art. 157 da Lei Complementar nº 214, de 2025.")]
        cct400001,

        [EnumValue("410001")]
        [Description("Fornecimento de bonificações quando constem do respectivo documento fiscal e que não dependam de evento posterior, observado o art. 5º da Lei Complementar nº 214, de 2025.")]
        cct410001,

        [EnumValue("410002")]
        [Description("Transferências entre estabelecimentos pertencentes ao mesmo contribuinte, observado o art. 6º da Lei Complementar nº 214, de 2025.")]
        cct410002,

        [EnumValue("410003")]
        [Description("Doações, observado o art. 6º da Lei Complementar nº 214, de 2025.")]
        cct410003,

        [EnumValue("410004")]
        [Description("Exportações de bens e serviços, observado o art. 8º da Lei Complementar nº 214, de 2025.")]
        cct410004,

        [EnumValue("410005")]
        [Description("Fornecimentos realizados pela União, pelos Estados, pelo Distrito Federal e pelos Municípios, observado o art. 9º da Lei Complementar nº 214, de 2025.")]
        cct410005,

        [EnumValue("410006")]
        [Description("Fornecimentos realizados por entidades religiosas e templos de qualquer culto, inclusive suas organizações assistenciais e beneficentes, observado o art. 9º da Lei Complementar nº 214, de 2025.")]
        cct410006,

        [EnumValue("410007")]
        [Description("Fornecimentos realizados por partidos políticos, inclusive suas fundações, entidades sindicais dos trabalhadores e instituições de educação e de assistência social, sem fins lucrativos, observado o art. 9º da Lei Complementar nº 214, de 2025.")]
        cct410007,

        [EnumValue("410008")]
        [Description("Fornecimentos de livros, jornais, periódicos e do papel destinado a sua impressão, observado o art. 9º da Lei Complementar nº 214, de 2025.")]
        cct410008,

        [EnumValue("410009")]
        [Description("Fornecimentos de fonogramas e videofonogramas musicais produzidos no Brasil contendo obras musicais ou literomusicais de autores brasileiros e/ou obras em geral interpretadas por artistas brasileiros, bem como os suportes materiais ou arquivos digitais que os contenham, salvo na etapa de replicação industrial de mídias ópticas de leitura a laser, observado o art. 9º da Lei Complementar nº 214, de 2025.")]
        cct410009,

        [EnumValue("410010")]
        [Description("Fornecimentos de serviço de comunicação nas modalidades de radiodifusão sonora e de sons e imagens de recepção livre e \r\n gratuita, observado o art. 9º da Lei Complementar nº 214, de 2025.")]
        cct410010,

        [EnumValue("410011")]
        [Description("Fornecimentos de ouro, quando definido em lei como ativo financeiro ou instrumento cambial, observado o art. 9º da Lei Complementar nº 214, de 2025.")]
        cct410011,

        [EnumValue("410012")]
        [Description("Fornecimento de condomínio edilício não optante pelo regime regular, observado o art. 26 da Lei Complementar nº 214, de 2025.")]
        cct410012,

        [EnumValue("410013")]
        [Description("Exportações de combustíveis, observado o art. 98 da Lei Complementar nº 214, de 2025.")]
        cct410013,

        [EnumValue("410014")]
        [Description("Fornecimento de produtor rural não contribuinte, observado o art. 164 da Lei Complementar nº 214, de 2025.")]
        cct410014,

        [EnumValue("410015")]
        [Description("Fornecimento por transportador autônomo não contribuinte, observado o art. 169 da Lei Complementar nº 214, de 2025.")]
        cct410015,

        [EnumValue("410016")]
        [Description("Fornecimento ou aquisição de resíduos sólidos, observado o art. 170 da Lei Complementar nº 214, de 2025.")]
        cct410016,

        [EnumValue("410017")]
        [Description("Aquisição de bem móvel com crédito presumido sob condição de revenda realizada, observado o art. 171 da Lei Complementar nº 214, de 2025.")]
        cct410017,

        [EnumValue("410018")]
        [Description("Operações relacionadas aos fundos garantidores e executores de políticas públicas, inclusive de habitação, previstos em lei, assim entendidas os serviços prestados ao fundo pelo seu agente operador e por entidade encarregada da sua administração, observado o art. 213 da Lei Complementar nº 214, de 2025.")]
        cct410018,

        [EnumValue("410019")]
        [Description("Exclusão da gorjeta na base de cálculo no fornecimento de alimentação, observado o art. 274 da Lei Complementar nº 214, de 2025.")]
        cct410019,

        [EnumValue("410020")]
        [Description("Exclusão do valor de intermediação na base de cálculo no fornecimento de alimentação, observado o art. 274 da Lei Complementar nº 214, de 2025.")]
        cct410020,

        [EnumValue("510001")]
        [Description("Operações, sujeitas a diferimento, com energia elétrica ou com direitos a ela relacionados, relativas à geração, comercialização, distribuição e transmissão, observado o art. 28 da Lei Complementar nº 214, de 2025.")]
        cct510001,

        [EnumValue("510002")]
        [Description("Operações, sujeitas a diferimento, com insumos agropecuários e aquícolas destinados a produtor rural contribuinte, observado o art. 138 da Lei Complementar nº 214, de 2025.")]
        cct510002,

        [EnumValue("550001")]
        [Description("Exportações de bens materiais, observado o art. 82 da Lei Complementar nº 214, de 2025.")]
        cct550001,

        [EnumValue("550002")]
        [Description("Regime de Trânsito, observado o art. 84 da Lei Complementar nº 214, de 2025.")]
        cct550002,

        [EnumValue("550003")]
        [Description("Regimes de Depósito, observado o art. 85 da Lei Complementar nº 214, de 2025.")]
        cct550003,

        [EnumValue("550004")]
        [Description("Regimes de Depósito, observado o art. 87 da Lei Complementar nº 214, de 2025.")]
        cct550004,

        [EnumValue("550005")]
        [Description("Regimes de Depósito, observado o art. 87 da Lei Complementar nº 214, de 2025.")]
        cct550005,

        [EnumValue("550006")]
        [Description("Regimes de Permanência Temporária, observado o art. 88 da Lei Complementar nº 214, de 2025.")]
        cct550006,

        [EnumValue("550007")]
        [Description("Regimes de Aperfeiçoamento, observado o art. 90 da Lei Complementar nº 214, de 2025.")]
        cct550007,

        [EnumValue("550008")]
        [Description("Importação de bens para o Regime de Repetro-Temporário, de que tratam o inciso I do art. 93 da Lei Complementar nº 214, de 2025.")]
        cct550008,

        [EnumValue("550009")]
        [Description("GNL-Temporário, de que trata o inciso II do art. 93 da Lei Complementar nº 214, de 2025.")]
        cct550009,

        [EnumValue("550010")]
        [Description("Repetro-Permanente, de que trata o inciso III do art. 93 da Lei Complementar nº 214, de 2025.")]
        cct550010,

        [EnumValue("550011")]
        [Description("Repetro-Industrialização, de que trata o inciso IV do art. 93 da Lei Complementar nº 214, de 2025.")]
        cct550011,

        [EnumValue("550012")]
        [Description("Repetro-Nacional, de que trata o inciso V do art. 93 da Lei Complementar nº 214, de 2025.")]
        cct550012,

        [EnumValue("550013")]
        [Description("Repetro-Entreposto, de que trata o inciso VI do art. 93 da Lei Complementar nº 214, de 2025.")]
        cct550013,

        [EnumValue("550014")]
        [Description("Zona de Processamento de Exportação, observado os arts. 99, 100 e 102 da Lei Complementar nº 214, de 2025.")]
        cct550014,

        [EnumValue("550015")]
        [Description("Regime Tributário para Incentivo à Modernização e à Ampliação da Estrutura Portuária - Reporto, observado o art. 105 da Lei Complementar nº 214, de 2025.")]
        cct550015,

        [EnumValue("550016")]
        [Description("Regime Especial de Incentivos para o Desenvolvimento da Infraestrutura - Reidi, observado o art. 106 da Lei Complementar nº 214, de 2025.")]
        cct550016,

        [EnumValue("550017")]
        [Description("Regime Tributário para Incentivo à Atividade Econômica Naval – Renaval, observado o art. 107 da Lei Complementar nº 214, de 2025.")]
        cct550017,

        [EnumValue("550018")]
        [Description("Desoneração da aquisição de bens de capital, , observado o art. 109 da Lei Complementar nº 214, de 2025.")]
        cct550018,

        [EnumValue("550019")]
        [Description("Importação de bem material por indústria incentivada para utilização na Zona Franca de Manaus, observado o art. 443 da Lei Complementar nº 214, de 2025.")]
        cct550019,

        [EnumValue("550020")]
        [Description("Áreas de livre comércio, observado o art. 461 da Lei Complementar nº 214, de 2025.")]
        cct550020,

        [EnumValue("620001")]
        [Description("Tributação monofásica sobre combustíveis, observado o art. 172 da Lei Complementar nº 214, de 2025.")]
        cct620001,

        [EnumValue("620002")]
        [Description("Tributação monofásica com responsabilidade pela retenção sobre combustíveis, observado o art. 178 da Lei Complementar nº 214, de 2025.")]
        cct620002,

        [EnumValue("620003")]
        [Description("Tributação monofásica com tributos retidos por responsabilidade sobre combustíveis, observado o art. 178 da Lei Complementar nº 214, de 2025.")]
        cct620003,

        [EnumValue("620004")]
        [Description("Tributação monofásica sobre mistura de EAC com gasolina A em percentual superior ou inferior ao obrigatório, observado o art. 179 da Lei Complementar nº 214, de 2025.")]
        cct620004,

        [EnumValue("620005")]
        [Description("Tributação monofásica sobre combustíveis cobrada anteriormente, observador o art. 180 da Lei Complementar nº 214, de 2025.")]
        cct620005,

        [EnumValue("800001")]
        [Description("Fusão, cisão ou incorporação, observado o art. 55 da Lei Complementar nº 214, de 2025.")]
        cct800001,

        [EnumValue("800002")]
        [Description("Transferência de crédito do associado, inclusive as cooperativas singulares, para cooperativa de que participa das operações antecedentes às operações em que fornece bens e serviços e os créditos presumidos, observado o art. 272 da Lei Complementar nº 214, de 2025.")]
        cct800002,

        [EnumValue("810001")]
        [Description("Crédito presumido sobre o valor apurado nos fornecimentos a partir da Zona Franca de Manaus, observado o art. 450 da Lei Complementar nº 214, de 2025.")]
        cct810001,

        [EnumValue("820001")]
        [Description("Documento com informações de fornecimento de serviços de planos de assinstência à saúde, mas com tributação realizada por outro meio, observado o art. 235 da Lei Complementar nº 214, de 2025.")]
        cct820001,

        [EnumValue("820002")]
        [Description("Documento com informações de fornecimento de serviços de planos de assinstência funerária, mas com tributação realizada por outro meio, observado o art. 236 da Lei Complementar nº 214, de 2025.")]
        cct820002,

        [EnumValue("820003")]
        [Description("Documento com informações de fornecimento de serviços de planos de assinstência à saúde de animais domésticos, mas com tributação realizada por outro meio, observado o art. 243 da Lei Complementar nº 214, de 2025.")]
        cct820003,

        [EnumValue("820004")]
        [Description("Documento com informações de prestação de serviços de consursos de prognósticos, mas com tributação realizada por outro meio, observado o art. 248 da Lei Complementar nº 214, de 2025.")]
        cct820004,

        [EnumValue("820005")]
        [Description("Documento com informações de alienação de bens imóveis, mas com tributação realizada por outro meio,, observado o art. 254 da Lei Complementar nº 214, de 2025.")]
        cct820005
    }
}
