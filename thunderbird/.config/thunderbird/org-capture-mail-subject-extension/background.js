async function handleClick() {
  let tabs = await browser.mailTabs.query({ active: true });
  if (tabs.length === 0) {
    console.log("No active mail tab found.");
    return;
  }

  let messages = await browser.mailTabs.getSelectedMessages();
  if (messages.messages.length > 0) {
    let subject = messages.messages[0].subject;

    // console.log("Focused email subject:", subject);
    console.log(subject);
    let response = await browser.runtime.sendNativeMessage(
      "org_capture_mail_subject",
      { subject: subject },
    );
  }
}

browser.commands.onCommand.addListener((command) => {
  if (command === "org-capture-mail-subject") {
    handleClick();
  }
});
