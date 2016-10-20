# Client: Superman
# Email: acarnine+client@influential.co
# Password: password

# Media Query at 1715 creates bizarre gap
# The below gets added to div.insight-list (child of div.insights)
.insight-list {
  padding-right: 360px;
}

# This is what we want the formatting to look like
@media only screen and (max-width: 107.143em)
.insight-list {
    padding-right: 0;
    margin-top: 50px;
}
