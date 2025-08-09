import asyncio
from playwright.async_api import async_playwright
import json
import traceback 

async def find_potential_apis(url, max_api_count=20):
    potential_apis = {} # URL'leri ve kısa açıklamaları saklamak için sözlük
    api_count = 0 # Tespit edilen API sayacını tutmak için

    async with async_playwright() as p:
        browser = await p.chromium.launch(headless=True) # Headless=True ile tarayıcıyı görünmez modda çalıştır
        page = await browser.new_page()

        print(f"'{url}' adresine gidiliyor ve potansiyel API istekleri dinleniyor...")

        # Yanıt dinleyici: Her ağ yanıtı geldiğinde bu fonksiyon çalışır
        page.on("response", lambda response: asyncio.create_task(process_response(response, potential_apis, max_api_count)))

        # Sayfayı yükle
        # timeout değerini arttırdık, bazı siteler uzun sürebilir.
        # "networkidle" daha kapsamlı bir bekleme, tüm ağ aktivitesi durana kadar bekler.
        try:
            await page.goto(url, wait_until="load", timeout=240000)
            await page.wait_for_timeout(240000) # Sayfanın JS'inin tamamen çalışması için ek bekleme
        except Exception as e:
            print(f"Sayfaya gidilirken veya yüklenirken hata oluştu: {e}")
            await browser.close()
            return list(potential_apis.keys()) # Şimdiye kadar bulunanları döndür

        # Opsiyonel: Daha fazla API'yi tetiklemek için sayfada gezinme veya etkileşim
        # try:
        #     # Örnek: Bir "Daha Fazla Göster" veya "Sonraki Sayfa" butonuna tıklama
        #     # Daha fazla API keşfetmek için siteye özel etkileşimler ekleyebilirsiniz.
        #     # await page.click("button.load-more-button")
        #     # await page.wait_for_timeout(2000)
        #     pass
        # except Exception as e:
        #     print(f"Ek etkileşim sırasında hata: {e}")

        await browser.close()

    return list(potential_apis.keys())

async def process_response(response, potential_apis, max_api_count):
    """Her ağ yanıtını kontrol eden yardımcı fonksiyon."""
    if len(potential_apis) >= max_api_count:
        return # Belirlenen API sayısına ulaşıldıysa daha fazla kontrol etme

    # Yalnızca başarılı HTTP yanıtlarını ve belirli içerik tiplerini incele
    if 200 <= response.status < 300: # Başarılı HTTP durum kodları
        content_type = response.headers.get("content-type", "").lower()

        # JSON veya diğer yapılandırılmış veri tiplerini arıyoruz
        if "application/json" in content_type or \
           "text/json" in content_type or \
           "application/xml" in content_type or \
           "text/xml" in content_type or \
           "text/plain" in content_type: # Bazı basit API'ler text/plain döndürebilir

            # URL'de yaygın API anahtar kelimelerini kontrol et
            # Bu filtreyi siteye göre özelleştirebilirsin
            potential_keywords = ["api", "data", "json", "query", "graphql", "svc", "service"]
            is_api_url = any(keyword in response.url.lower() for keyword in potential_keywords)

            # JSON yanıtlarını ve anahtar kelime içeren URL'leri önceliklendir
            if "application/json" in content_type or is_api_url:
                try:
                    # Yanıt gövdesini al
                    response_body = await response.json() # JSON olarak parse etmeye çalış
                    # Burada API'nin yapısını veya önemli bir kısmını inceleyebiliriz
                    # Örneğin, ilk anahtarları veya listenin ilk öğesini gösterebiliriz.
                    
                    description = f"JSON yanıt, boyutu: {len(str(response_body))} karakter."
                    if isinstance(response_body, dict):
                        description += f" İlk anahtarlar: {list(response_body.keys())[:3]}"
                    elif isinstance(response_body, list) and len(response_body) > 0:
                        description += f" Liste, ilk öğe tipi: {type(response_body[0]).__name__}"
                        
                    if response.url not in potential_apis:
                        potential_apis[response.url] = description
                        print(f"Potansiyel API tespit edildi: {response.url} ({description})")
                except Exception as e:
                    # JSON değilse veya parse edilemezse, yine de kaydet (ama daha düşük öncelikle)
                    # traceback.print_exc() # Hata ayıklama için kullanılabilir
                    if is_api_url and response.url not in potential_apis:
                         potential_apis[response.url] = f"Muhtemelen API (URL anahtar kelime içeriyor). Content-Type: {content_type}"
                         print(f"Potansiyel API (URL anahtar kelime içeriyor): {response.url}")
                    elif "application/json" in content_type and response.url not in potential_apis:
                         potential_apis[response.url] = f"JSON Content-Type, ancak JSON parse edilemedi. Content-Type: {content_type}"
                         print(f"Potansiyel API (JSON Content-Type, parse edilemedi): {response.url}")

# Ana fonksiyonu çalıştır
if __name__ == "__main__":
    # Test etmek istediğiniz URL'leri buraya ekleyebilirsiniz.
    # Genel bir ana sayfa, birçok API çağrısını tetiklemeyebilir.
    # Genellikle spesifik bir ürün, profil veya arama sonucu sayfası daha fazla API kullanır.
    urls_to_scan = [
      "https://www.transfermarkt.com/arda-guler/profil/spieler/861410"#,
      # "https://www.transfermarkt.com/arda-guler/leistungsdaten/spieler/861410/plus/0?saison=2024",
      # "https://www.transfermarkt.com/arda-guler/marktwertverlauf/spieler/861410",
      # "https://www.transfermarkt.com/arda-guler/verletzungen/spieler/861410",
      # "https://www.transfermarkt.com/arda-guler/ausfaelle/spieler/861410",
      # "https://www.transfermarkt.com/arda-guler/nationalmannschaft/spieler/861410",
      #"https://www.transfermarkt.com/uefa-champions-league/startseite/pokalwettbewerb/CL",
      # "https://www.transfermarkt.com/uefa-europa-league/startseite/pokalwettbewerb/EL",
      # "https://www.transfermarkt.com/uefa-euro/startseite/pokalwettbewerb/EURO",
      # "https://www.transfermarkt.com/international-friendlies/startseite/pokalwettbewerb/FS"
      # "https://www.transfermarkt.com/spielbericht/index/spielbericht/4343512",
      # "https://www.transfermarkt.com/borussia-dortmund_real-madrid/aufstellung/spielbericht/4343512"
      # "https://www.transfermarkt.com/",
      # "https://www.transfermarkt.com/statistik/topscorer",
      # "https://www.transfermarkt.com/navigation/wettbewerbe",
      # "https://www.transfermarkt.com/wettbewerbe/europa",
      # "https://www.transfermarkt.com/wettbewerbe/fifa",
      # "https://www.transfermarkt.com/wettbewerbe/europaJugend",
      # "https://www.transfermarkt.com/ligue-1/startseite/wettbewerb/FR1",
      # "https://www.transfermarkt.com/fc-paris-saint-germain/startseite/verein/583",
      # "https://www.transfermarkt.com/paris-saint-germain/spielplan/verein/583/plus/0?saison_id=2024",
      # "https://www.transfermarkt.com/ligue-1/gesamtspielplan/wettbewerb/FR1/saison_id/2024",
      # "https://www.transfermarkt.com/paris-saint-germain/spielplandatum/verein/583/plus/0?saison_id=2024&wettbewerb_id=&day=&heim_gast=&punkte=&datum_von=&datum_bis=",
      # "https://www.transfermarkt.com/navigation/transfersundgeruechte",
      # "https://www.transfermarkt.com/navigation/marktwerte",
      # "https://www.transfermarkt.com/navigation/statistiken"

    ]
    all_found_apis = {}

    for url in urls_to_scan:
        print(f"\n--- {url} adresindeki API'ler taranıyor ---")
        found_apis_for_url = asyncio.run(find_potential_apis(url))
        if found_apis_for_url:
            all_found_apis[url] = found_apis_for_url
        else:
            all_found_apis[url] = ["Bu sayfada belirgin bir API bulunamadı."]


    print("\n\n--- Tüm Sayfalar İçin Tespit Edilen Potansiyel API URL'leri ve Açıklamaları ---")
    for scanned_url, apis in all_found_apis.items():
        print(f"\nURL: {scanned_url}")
        if isinstance(apis, list):
            for api_url in apis:
                print(f"  - {api_url}")
        else: # Sözlük olarak geliyorsa
            for api_url, desc in apis.items():
                print(f"  - {api_url} ({desc})")

    print("\nNot: Tespit edilen bu URL'ler her zaman herkesin kullanabileceği 'ücretsiz' API'ler olmayabilir.")
    print("Çoğu site, bu dahili API'leri kendi kullanıcı arayüzünü beslemek için kullanır.")
    print("Daha fazla bilgi için sitenin 'Geliştiriciler' veya 'API Belgeleri' bölümlerini kontrol edin.")

